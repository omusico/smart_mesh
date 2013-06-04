-module(smart_mesh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _Pid} = rproxy:start_link(),
  smart_mesh_sup:start_link().

stop(_State) ->
  ok.
