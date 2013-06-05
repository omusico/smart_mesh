-module(smart_mesh_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {ReqMethod, Req} = cowboy_req:method(Req),
  {ReqHeadersBin, Req} = cowboy_req:headers(Req),
  {PathBin, Req} = cowboy_req:path(Req),
  {QueryStringBin, Req} = cowboy_req:qs(Req),

  Url = string:join(["http://127.0.0.1:1986",
    binary_to_list(PathBin),
    "?",
    binary_to_list(QueryStringBin)], ""),

  {ok, {{_HttpVsn, StatusCode, _ReasonPhrase}, RspHeaders, RspBody}} = httpc:request(
    http_method(ReqMethod),
    {Url, req_headers(ReqHeadersBin)},
    [],
    [{body_format, binary}]),
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

