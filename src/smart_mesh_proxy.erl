-module(smart_mesh_proxy).
-behaviour(gen_server).

-export([start_link/0]).
-export([insert/3]).
-export([lookup/1]).
-export([delete/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  Dispatch = cowboy_router:compile([
              %% {URIHost, list({URIPath, Handler, Opts})}
              {'_', [{'_', smart_mesh_cowboy_handler, []}]}
            ]),
  %% Name, NbAcceptors, TransOpts, ProtoOpts
  cowboy:start_http(
    smart_mesh_http_listener,
    100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, dict:new()}.

insert(HostName, Server, Port) ->
  gen_server:call(?SERVER, {insert, {HostName, {Server, Port}}}).

lookup(HostName) ->
  gen_server:call(?SERVER, {lookup, HostName}).

delete(HostName, Server, Port) ->
  gen_server:call(?SERVER, {delete, {HostName, {Server, Port}}}).

handle_call({lookup, HostName}, _From, State) ->
  case dict:find(HostName, State) of
    {ok, Dynos} ->
      {reply, find_best_dyno(HostName, Dynos), State};
    error ->
      {reply, {error, not_found}, State}
  end;

handle_call({insert, {HostName, {Server, Port}}}, _From, State) ->
  NewState = case dict:find(HostName, State) of
    {ok, Servers} ->
      dict:store(HostName, [{Server, Port}|Servers], State);
    error ->
      dict:store(HostName, [{Server, Port}], State)
  end,
  {reply, ok, NewState};

handle_call({delete, {HostName, {Server, Port}}}, _From, State) ->
  case dict:find(HostName, State) of
    {ok, Servers} ->
      NewState = dict:store(HostName,
                            lists:delete({Server, Port}, Servers),
                            State),
      {reply, ok, NewState};
    error ->
      % nothing to delete
      {reply, ok, State}
  end.

handle_cast(_M, State) ->
  {noreply, State}.

handle_info(_M, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

find_best_dyno(HostName, Dynos) ->
  F = fun({Server, Port}) ->
        case smart_mesh_dynoman_center:lookup(Server) of
          {ok, {Server, Load}} ->
            {Load, {Server, Port}}
        end
      end,
  {_Load, {Server, Port}} = lists:min(lists:map(F, Dynos)),
  {ok, {HostName, {Server, Port}}}.

