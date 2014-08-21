-module(red_fields_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  rf_config:init(),
  {Units, Map} = rf_config:read_config(),
  io:format("Map: ~p~n", [Map]),
  rf_map:init(Map),
  MapSize = rf_map:game_map_size(Map),
  red_fields_sup:start_link(Units, MapSize).

stop(_State) ->
  ok.

