-module(domain_center).
-behaviour(gen_server).

% API
-export([start_link/0]).

-export([insert_domain/3]).
-export([lookup_domain/1]).
-export([update_host_load/2]).

% Callback
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {domain_to_hosts, host_loads}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{domain_to_hosts = dict:new(),
      host_loads = ets:new(host_loads, [set])}}.

insert_domain(Domain, Host, Port) ->
  gen_server:cast(?SERVER, {insert_domain, {Domain, Host, Port}}).

lookup_domain(Domain) ->
  gen_server:call(?SERVER, {lookup_domain, Domain}).

update_host_load(Host, Load) ->
  gen_server:cast(?SERVER, {update_host_load, {Host, Load}}).

handle_call({lookup_domain, Domain}, _From, State) ->
  case dict:find(Domain, State#state.domain_to_hosts) of
    {ok, HostWithPorts} ->
      {reply, find_best_host(Domain, HostWithPorts, State), State};
    error ->
      {reply, {error, not_found}, State}
  end.

handle_cast({update_host_load, {Host, Load}}, State) ->
  ets:insert(State#state.host_loads, {Host, Load}),
  {noreply, State};

handle_cast({insert_domain, {Domain, Host, Port}}, #state{domain_to_hosts = DomainToHosts} = State) ->
  case dict:find(Domain, DomainToHosts) of
    {ok, Hosts} ->
      % add host:port to existing domain
      NewDict = dict:store(Domain, [{Host, Port}|Hosts], DomainToHosts),
      {noreply, State#state{domain_to_hosts = NewDict}};
    error ->
      % insert new domain
      NewDict = dict:store(Domain, [{Host, Port}], DomainToHosts),
      {noreply, State#state{domain_to_hosts = NewDict}}
  end.

handle_info(_M, State) ->
  {noreply, {ok}, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% private
find_best_host(Domain, HostWithPorts, State) ->
  HostWithLoads = get_host_with_load(HostWithPorts, State#state.host_loads),
  {_Load, Host, Port} = lists:min(HostWithLoads),
  {ok, {Domain, Host, Port}}.

get_host_with_load(HostWithPorts, HostLoads) ->
  F = fun({Host, Port}) ->
        case ets:lookup(HostLoads, Host) of
          [{Host, Load}] ->
            {Load, Host, Port}
        end
      end,
 lists:map(F, HostWithPorts).

