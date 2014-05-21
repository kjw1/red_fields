-module(rf_map).

-include("rf_terrain.hrl").

-export([init/1]).

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





