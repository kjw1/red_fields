-module(rf_map).

-include("rf_terrain.hrl").

-export([init/1, get_terrain/4]).

-record(map_chunk, {pos, terrain}).

-define(CHUNK_SIZE, 5).

init(MapData) ->
  ets:new(map_chunks, [{read_concurrency, true},
                        named_table,
                        public,
                        set,
                        {keypos, #map_chunk.pos}]),
  store_data(MapData).

store_data([ARow | _]=MapData) ->
  RowStarts = lists:seq(1, length(ARow), ?CHUNK_SIZE),
  ColumnStarts = lists:seq(1, length(MapData), ?CHUNK_SIZE),
  io:format("Rows ~p Columns ~p~n", [RowStarts, ColumnStarts]),
  Chunks = [ {Row, Column} || Row <- RowStarts, Column <- ColumnStarts],
  lists:foreach(fun(Chunk) -> store_chunk(Chunk, MapData) end, Chunks).

store_chunk({Row, Column}=Chunk, Map) ->
  io:format("Storing chunk: ~p~n", [Chunk]),
  ChunkData = rf_grid:get_chunk(Row, Column, ?CHUNK_SIZE, ?CHUNK_SIZE, Map),
  ets:insert(map_chunks, #map_chunk{pos={Row div ?CHUNK_SIZE, Column div ?CHUNK_SIZE }, terrain=ChunkData}).

get_terrain(X, Y, Width, Height) ->
  XStart      = X      div ?CHUNK_SIZE,
  ChunkWidth  = Width  div ?CHUNK_SIZE,
  YStart      = Y      div ?CHUNK_SIZE,
  ChunkHeight = Height div ?CHUNK_SIZE,
  RowStarts = lists:seq(XStart, XStart + ChunkWidth),
  ColumnStarts = lists:seq(YStart, YStart + ChunkHeight),
  Chunks = [ {Row, Column} || Row <- RowStarts, Column <- ColumnStarts],
  io:format("Combining from chunks: ~p~n", [Chunks]),
  lists:foldl(fun(Chunk, StitchedChunks) ->
        combine_chunks(Chunk, StitchedChunks, X, Y, Width, Height)
    end, {{XStart, YStart}, none}, Chunks).
    
    
combine_chunks(ChunkPos, StitchedChunks, X, Y, Width, Height) ->
  {ChunkX, ChunkY, ChunkWidth, ChunkHeight} = get_overlap(ChunkPos, X, Y, Width, Height),
  [#map_chunk{terrain=MapChunk}] = ets:lookup(map_chunks, ChunkPos),
  MapPiece = rf_grid:get_chunk(ChunkX, ChunkY, ChunkWidth, ChunkHeight, MapChunk), 
  stitch_rows(ChunkPos, MapPiece, StitchedChunks).

get_overlap({ChunkPosX, ChunkPosY}, X, Y, Width, Height) ->
  ChunkX = ?CHUNK_SIZE * ChunkPosX + 1,
  ChunkY = ?CHUNK_SIZE * ChunkPosY + 1,
  ChunkXMax = ChunkX + ?CHUNK_SIZE - 1,
  ChunkYMax = ChunkY + ?CHUNK_SIZE - 1,
  Result = {max(ChunkX, X),
            max(ChunkY, Y),
            min(ChunkXMax, X + Width -1),
            min(ChunkYMax, Y + Height -1)},
  io:format("Overlap: ~p~n", [Result]),
  Result.

stitch_rows(Start, MapPiece, {Start, none}) ->
  {Start, MapPiece};
stitch_rows(ChunkPos, MapPiece, {Start, StitchedRows}) ->
  StitchStart= get_stitch_start_row(ChunkPos, Start),
  {Start, do_stitch_at(StitchStart, MapPiece, StitchedRows)}.

get_stitch_start_row({XPos, _}, {XStart, _}) ->
  XPos - XStart.

do_stitch_at(StitchStart, MapPiece, StitchedRows) when StitchStart > length(StitchedRows) ->
  lists:concat([StitchedRows, MapPiece]);
do_stitch_at(StitchStart, MapPiece, StitchedRows) ->
  {Head, Middle, Tail} = rf_grid:get_middle(StitchStart, length(MapPiece), StitchedRows),
  ListPairs = lists:zip(Middle, MapPiece),
  NewMiddle = lists:map(fun({RowBase, RowEnd}) -> lists:concat([RowBase, RowEnd]) end, ListPairs),
  lists:concat([Head, NewMiddle, Tail]).


