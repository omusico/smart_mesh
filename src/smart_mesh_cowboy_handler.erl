-module(smart_mesh_cowboy_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {ReqMethodBin, Req} = cowboy_req:method(Req),
  {ReqHeadersBin, Req} = cowboy_req:headers(Req),
  {PathBin, Req} = cowboy_req:path(Req),
  {QueryStringBin, Req} = cowboy_req:qs(Req),

  {HostBin, Req} = cowboy_req:host(Req),
  io:format("HOST: ~p~n", [HostBin]),

  Host = binary_to_list(HostBin),
  {ok, {Host, {Server, Port}}} = smart_mesh_proxy:lookup(Host),
  io:format("UPSTREAM: ~p -> [~p, ~p]~n", [Host, Server, Port]),

  Url = upstream_url(Server, Port,
                    binary_to_list(PathBin),
                    binary_to_list(QueryStringBin)),

  ReqMethod = http_method(ReqMethodBin),

  {ok, {{_HttpVsn, StatusCode, _ReasonPhrase}, RspHeaders, RspBody}} =
    case ReqMethod of
      get ->
        httpc:request(
          ReqMethod,
          {Url, req_headers(ReqHeadersBin)},
          [],
          [{body_format, binary}]);
      post ->
        {ok, BodyBin, _Req} = cowboy_req:body(Req),

        httpc:request(
          ReqMethod,
          {
            Url,
            req_headers(ReqHeadersBin),
            "application/x-www-form-urlencoded",
            BodyBin
          },
          [],
          [{body_format, binary}])
    end,

  {ok, Req2} = cowboy_req:reply(StatusCode, RspHeaders, RspBody, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

http_method(<<"HEAD">>) ->
  head;
http_method(<<"GET">>) ->
  get;
http_method(<<"PUT">>) ->
  put;
http_method(<<"POST">>) ->
  post;
http_method(<<"TRACE">>) ->
  trace;
http_method(<<"OPTIONS">>) ->
  options;
http_method(<<"DELETE">>) ->
  delete.

req_headers(ReqHeadersBin) ->
  F = fun({K, V}) ->
      {binary_to_list(K), binary_to_list(V)}
  end,
  lists:map(F, ReqHeadersBin).

upstream_url(Host, Port, Path, []) ->
  string:join(["http://", Host, ":", integer_to_list(Port), Path], "");

upstream_url(Host, Port, Path, QueryString) ->
  string:join(["http://", Host, ":", integer_to_list(Port), Path, "?", QueryString], "").

