-module(smart_mesh_dynoman_center).
-behaviour(gen_server).

-export([start_link/0]).
-export([insert/2]).
-export([lookup/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, ets:new(dyno_load, [set])}.

insert(Server, Load) ->
  gen_server:call(?SERVER, {insert, {Server, Load}}).

lookup(Server) ->
  gen_server:call(?SERVER, {lookup, Server}).

handle_call({insert, {Server, Load}}, _From, State) ->
  ets:insert(State, {Server, Load}),
  {reply, ok, State};

handle_call({lookup, Server}, _From, State) ->
  case ets:lookup(State, Server) of
      [{Server, Load}] ->
        {reply, {ok, {Server, Load}}, State};
      [] ->
        {reply, {error, not_found}, State}
  end.

handle_cast(_M, State) ->
  {noreply, State}.

handle_info(_M, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

