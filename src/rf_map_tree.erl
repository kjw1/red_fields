-module(rf_map_tree).
-export([to_list/1]).

-include("include/rf_map_tree.hrl").

to_list(#rf_map_tree{nw=NW, ne=NE, sw=SW, se=SE}) ->
  [ NW, NE, SW, SE ].
