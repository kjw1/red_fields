-module(rf_config).

-include("rf_terrain.hrl").

-export([init/0, read_config/0]).

init() ->
  ets:new(unit_types, [{read_concurrency, true},
                          named_table,
                          public,
                          set,
                          {keypos, #rf_unit_type.id}]),
  ets:new(terrain_types, [{read_concurrency, true},
                          named_table,
                          public,
                          set,
                          {keypos, #rf_terrain_type.id}]).

read_config() ->
  PrivDir = code:priv_dir(red_fields),
  {ok, Config} = file:consult(filename:join(PrivDir, "game_data")),
  configure_terrain_types(Config),
  configure_unit_types(Config),
  Units = configure_units(Config),
  Map = configure_map(Config),
  {Units, Map}.

configure_units(Config) ->
  Units = lists:keyfind(units, 1, Config),
  case Units of
    false -> [];
    {units, UnitList} -> UnitList
  end.

configure_unit_types(Config) ->
  {unit_types, Types} = lists:keyfind(unit_types, 1, Config),
  lists:foreach(fun({TypeName}) ->
        ets:insert(unit_types, #rf_unit_type{id=TypeName})
    end, Types).

configure_terrain_types(Config) ->
  Types = lists:filter(fun(Item) -> element(1, Item) =:= terrain_type end, Config),
  lists:foreach(fun({terrain_type, Id, Speed}) ->
                  io:format("Adding terrain type ~p: ~p~n", [Id, Speed]),
                  ets:insert(terrain_types, #rf_terrain_type{id=Id, speed_mult=Speed})
                end, Types).

configure_map(Config) ->
  {map, Width, Height, Filler, Shapes} = lists:keyfind(map, 1, Config),
  Grid = generate_grid(Width, Height, Filler),
  lists:foldl(fun(Blob, AccGrid) ->
                  io:format("Adding blob type ~p~n", [Blob]),
                  add_blob(Blob, AccGrid)
                end, Grid, Shapes).

add_blob({Type, BlobX, BlobY, BlobWidth, BlobHeight}, Grid) ->
  {Head, Replace, Tail} = rf_grid:get_middle(BlobY, BlobHeight, Grid),
  NewMiddle = lists:map(fun(Row) ->
          {RowHead, RowReplace, RowTail} = rf_grid:get_middle(BlobX, BlobWidth, Row),
          NewRowMiddle = lists:map(fun(_) -> Type end, RowReplace),
          lists:append([RowHead, NewRowMiddle, RowTail])
      end, Replace),
  lists:append([Head, NewMiddle, Tail]).

generate_grid(Width, Height, Filler) ->
  lists:map(fun(_) ->
        lists:map(fun(_) ->
              Filler
          end, lists:seq(1, Width))
    end, lists:seq(1, Height)).
