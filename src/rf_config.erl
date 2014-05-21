-module(rf_config).

-include("rf_terrain.hrl").

-export([init/0, read_config/0]).

init() ->
  ets:new(terrain_types, [{read_concurrency, true},
                          named_table,
                          public,
                          set,
                          {keypos, #rf_terrain_type.id}]).

read_config() ->
  PrivDir = code:priv_dir(red_fields),
  {ok, Config} = file:consult(filename:join(PrivDir, "game_data")),
  configure_terrain_types(Config),
  configure_map(Config).

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
  {Head, Replace, Tail} = get_middle(BlobX, BlobWidth, Grid),
  NewMiddle = lists:map(fun(Column) ->
          {ColHead, ColReplace, ColTail} = get_middle(BlobY, BlobHeight, Column),
          NewColMiddle = lists:map(fun(_) -> Type end, ColReplace),
          lists:append([ColHead, NewColMiddle, ColTail])
      end, Replace),
  lists:append([Head, NewMiddle, Tail]).

get_middle(Start, Length, List) ->
  {Head, LongReplace} = lists:split(Start-1, List),
  {Replace, Tail} = lists:split(Length, LongReplace),
  {Head, Replace, Tail}.

generate_grid(Width, Height, Filler) ->
  lists:map(fun(_) ->
        lists:map(fun(_) ->
              Filler
          end, lists:seq(1, Height))
    end, lists:seq(1, Width)).
