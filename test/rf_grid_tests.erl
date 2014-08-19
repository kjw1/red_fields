-module(rf_grid_tests).

-include_lib("eunit/include/eunit.hrl").

safe_split_test() ->
  {[], [element]} = ?debugVal(rf_grid:safe_split(0, [element])),
  {[element], []} = ?debugVal(rf_grid:safe_split(1, [element])),
  {[1,2,3,4], [5]} = ?debugVal(rf_grid:safe_split(4, [1,2,3,4,5])).

get_middle_test() ->
  {[1,2,3,4], [5], []} = ?debugVal(rf_grid:get_middle(5, 1, [1,2,3,4,5])).

get_chunk_test() ->
  Map = test_utils:make_map(5,5),
  [[{1,1}]] = rf_grid:get_chunk(1, 1, 1, 1, Map),
  [[{5,1}]] = rf_grid:get_chunk(5, 1, 1, 1, Map),
  [[{1,1}, {2,1}]] = rf_grid:get_chunk(1, 1, 2, 1, Map),
  [[{1,1}], [{1,2}]] = rf_grid:get_chunk(1, 1, 1, 2, Map).

