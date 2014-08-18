-module(rf_map_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

map_test_() ->
  {setup,
    fun() ->
      MapData = test_utils:make_map(10, 5),
      rf_map:init(MapData),
      MapData
    end,
    fun(_State) ->
      ets:delete(map_chunks)
    end,
    fun(_Map) ->
      [fun test_get_overlap/0,
       fun test_init/0,
       fun test_get_chunk_piece/0,
       fun test_get_terrain/0
      ]
    end
  }.


test_init()  ->
  Grid1 = test_utils:make_map(5, 5),
  Grid2 = lists:map(fun(Row) ->
            lists:map(fun({X, Y}) -> {X+5, Y} end, Row)
          end, Grid1),
  [{map_chunk, {0, 0}, Grid1}] = ets:lookup(map_chunks, {0, 0}),
  [{map_chunk, {1, 0}, Grid2}] = ets:lookup(map_chunks, {1, 0}).

test_get_terrain() ->
  Retreived = ?debugVal(rf_map:get_terrain(5, 1, 2, 1)),
  [[{5,1}, {6, 1}]] = Retreived.

test_get_overlap() ->
  {4, 1, 5, 1} = ?debugVal(rf_map:get_overlap({0, 0}, 4, 1, 3, 1)),
  {6, 1, 6, 1} = ?debugVal(rf_map:get_overlap({1, 0}, 4, 1, 3, 1)),
  {6, 1, 6, 1} = ?debugVal(rf_map:get_overlap({1, 0}, 6, 1, 1, 1)).

test_get_chunk_piece() ->
  [[{6, 1}]] = rf_map:get_chunk_piece({1, 0}, 6, 1, 1, 1),
  [[{6, 1}, {6,2}]] = rf_map:get_chunk_piece({1, 0}, 6, 1, 2, 1),
  [[{2, 2}]] = rf_map:get_chunk_piece({0, 0}, 2, 2, 1, 1).
