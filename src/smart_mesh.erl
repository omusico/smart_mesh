-module(smart_mesh).

-export([start/0]).
-export([stop/0]).

start() ->
  ok = inets:start(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(smart_mesh).

stop() ->
  ok = application:stop(smart_mesh),
  ok = application:stop(cowboy),
  ok = application:stop(ranch),
  ok = application:stop(crypto),
  ok = inets:stop().

