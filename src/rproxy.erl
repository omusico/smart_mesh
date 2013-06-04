-module(rproxy).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_response/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(BUFSIZE, 1024 * 4).
-define(SERVER, ?MODULE).

start_link() ->
  Dispatch = cowboy_router:compile([
              %% {URIHost, list({URIPath, Handler, Opts})}
              {'_', [{'_', smart_mesh_handler, []}]}
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
  {ok, []}.

get_response(Host, Port, Req) ->
  gen_server:call(?SERVER, {get_response, {Host, Port, Req}}).

handle_call({get_response, {Host, Port, Req}}, _From, State) ->
  % TODO: use httpc:request() to send HTTP request
  {ok, Sock} = gen_tcp:connect(Host, Port, [{active, false}]),
  gen_tcp:send(Sock, Req),
  {reply, collect_response(Sock), State}.

handle_cast(_M, State) ->
  {noreply, State}.

handle_info(_M, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _extra) ->
  {ok, State}.

collect_response(Sock) ->
  collect_response(Sock, []).

collect_response(Sock, Data) ->
  case gen_tcp:recv(Sock, ?BUFSIZE) of
    {ok, Result} ->
      collect_response(Sock, [Result|Data]);
    {error, closed} ->
      {ok, lists:reverse(Data)}
  end.

