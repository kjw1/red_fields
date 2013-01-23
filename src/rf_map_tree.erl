-module(rf_map_tree).

-include("include/rf_map_tree").

to_list(#map_tree{nw=NW, ne=NE, sw=SW, se=SE}) ->
  [ NW, NE, SW, SE ].
