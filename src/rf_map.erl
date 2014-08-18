-module(rf_map).

-include("rf_terrain.hrl").

-export([init/1, get_terrain/4]).

-record(map_chunk, {pos, terrain}).

-define(CHUNK_SIZE, 5).

-ifdef(TEST).
-compile(export_all).
-endif.

init(MapData) ->
  ets:new(map_chunks, [{read_concurrency, true},
                        named_table,
                        public,
                        set,
                        {keypos, #map_chunk.pos}]),
  %io:format("Map data: ~p~n", [MapData]),
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
  XStart = (X - 1) div ?CHUNK_SIZE,
  XEnd   = (X - 1 + Width - 1) div ?CHUNK_SIZE,
  YStart = (Y - 1) div ?CHUNK_SIZE,
  YEnd   = (Y - 1 + Height - 1)  div ?CHUNK_SIZE,
  io:format("X: ~p Y: ~p XEnd: ~p YEnd: ~p~n", [XStart, YStart, XEnd, YEnd]),
  RowStarts = lists:seq(XStart, XEnd),
  ColumnStarts = lists:seq(YStart, YEnd),
  lists:foldl(fun(YChunk, AllRows) ->
    NewRows = lists:foldl(fun(XChunk, []) ->
        io:format("Getting chunk ~p ~p~n", [XChunk, YChunk]),
        ChunkPiece = get_chunk_piece({XChunk, YChunk}, X, Y, Width, Height),
        io:format("Got piece ~p~n", [ChunkPiece]),
        ChunkPiece;
      (XChunk, Columns) ->
        io:format("Getting chunk ~p ~p~n", [XChunk, YChunk]),
        ChunkPiece = get_chunk_piece({XChunk, YChunk}, X, Y, Width, Height),
        io:format("Got piece ~p~n", [ChunkPiece]),
        [ lists:concat([OldColumns, NewColumns]) ||
          {OldColumns, NewColumns} <- lists:zip(Columns, ChunkPiece)]
    end, [], RowStarts),
    lists:concat([AllRows, NewRows])
  end, [], ColumnStarts).

    
get_chunk_piece(ChunkPos, X, Y, Width, Height) ->
  {ChunkX, ChunkY, ChunkXEnd, ChunkYEnd} = get_overlap(ChunkPos, X, Y, Width, Height),
  [#map_chunk{terrain=MapChunk}] = ets:lookup(map_chunks, ChunkPos),
  io:format("x ~p y ~p w ~p h ~p~n", [ChunkX rem ?CHUNK_SIZE + 1,
                    ChunkY rem ?CHUNK_SIZE + 1,
                    ChunkXEnd - ChunkX+ 1, ChunkYEnd - ChunkY+ 1]),
  rf_grid:get_chunk(ChunkX rem ?CHUNK_SIZE + 1,
                    ChunkY rem ?CHUNK_SIZE + 1,
                    ChunkXEnd - ChunkX+ 1, ChunkYEnd - ChunkY+ 1, MapChunk).

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

