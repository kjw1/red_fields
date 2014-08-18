-module(rf_grid_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

safe_split_test() ->
  rf_grid:safe_split(0, [element]) =:= [element].

get_chunk_test() ->
  Map = test_utils:make_map(4,4),
  [[{1,1}]] = rf_grid:get_chunk(1, 1, 1, 1, Map),
  [[{1,1}, {2,1}]] = rf_grid:get_chunk(1, 1, 2, 1, Map),
  [[{1,1}], [{1,2}]] = rf_grid:get_chunk(1, 1, 1, 2, Map).

