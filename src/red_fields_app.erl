-module(red_fields_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  rf_config:init(),
  Map = rf_config:read_config(),
  io:format("Map: ~p~n", [Map]),
  red_fields_sup:start_link().

stop(_State) ->
  ok.
