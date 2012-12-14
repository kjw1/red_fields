-module(rf_map_segment).
-include("include/rf_terrain.hrl").
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([terrain_at/3]).
-export([fake_def/1]).

fake_def(Size) ->
  SeedList = lists:seq(1, Size),
  Terrain = lists:map(fun (Y) -> lists:map(fun(X) -> #rf_terrain{type={X, Y}} end, lists:seq(1,Size)) end, SeedList),
  #rf_map_segment{x=0,y=0, width=Size, height=Size, terrain=Terrain}.

% This might be better done as a DHT...

start_link(SegmentDef) ->
  gen_server:start_link(?MODULE, SegmentDef, []).

init(#rf_map_segment{x=X, y=Y, width=Width, height=Height} = Segment) 
    when Width > 200; Height > 200 ->
  {NW, NE, SW, SE} = split_segment(Segment),
  {ok, NWServer} = start_link(NW),
  {ok, NEServer} = start_link(NE),
  {ok, SWServer} = start_link(SW),
  {ok, SEServer} = start_link(SE),
  Tree = #rf_map_tree{ nw=NWServer, ne=NEServer, sw=SWServer, se=SEServer},
  {ok, #rf_map_segment{x=X, y=Y, width=Width, height=Height, terrain=Tree}};
init(SegmentDef) ->
  {ok, SegmentDef}.

handle_call(state, _From, State) ->
  {reply, State, State};
handle_call({get_terrain, X, Y}, From, MapSegment) ->
  create_get_terrain_result(get_terrain_at(X, Y, MapSegment, From), MapSegment).

handle_cast({get_terrain, X, Y, From}, MapSegment) ->
  handle_passed_terrain_at(get_terrain_at(X, Y, MapSegment, From), From, MapSegment).

terrain_at(Segment, X, Y) ->
  gen_server:call(Segment, {get_terrain, X, Y}).

handle_passed_terrain_at(passed, _From, MapSegment) ->
  {noreply, MapSegment};
handle_passed_terrain_at(Terrain, From, MapSegment) ->
  gen_server:reply(From, Terrain),
  {noreply, MapSegment}.

%Passes terrain at call to another process
terrain_at_pass(Segment, X, Y, From) ->
  gen_server:cast(Segment, {get_terrain, X, Y, From}),
  passed.

create_get_terrain_result(passed, MapSegment) ->
  {noreply, MapSegment};
create_get_terrain_result(Result, MapSegment) ->
  {reply, Result, MapSegment}.

%will either return passed or the terrain at that location
get_terrain_at(X, Y, #rf_map_segment{terrain=Terrain}= Segment, From) when is_record(Terrain, rf_map_tree)->
  get_terrain_from_children(X, Y, From, Segment);
get_terrain_at(X, Y, #rf_map_segment{x=StartX, y=StartY, terrain=Terrain}, _From) ->
  lists:nth(X- StartX, lists:nth(Y-StartY, Terrain)).

%Always returns 'passed'
get_terrain_from_children(X, Y, From, #rf_map_segment{x=StartX, y=StartY, width=Width, height=Height, terrain = Terrain}) when X - StartX < Width div 2, Y - StartY < Height div 2 ->
  terrain_at_pass(Terrain#rf_map_tree.nw, X, Y, From);
get_terrain_from_children(X, Y, From, #rf_map_segment{x=StartX, y=StartY, width=Width, height=Height, terrain = Terrain}) when X - StartX >= Width div 2, Y - StartY < Height div 2 ->
  terrain_at_pass(Terrain#rf_map_tree.ne, X, Y, From);
get_terrain_from_children(X, Y, From, #rf_map_segment{x=StartX, y=StartY, width=Width, height=Height, terrain = Terrain}) when X - StartX < Width div 2, Y - StartY >= Height div 2 ->
  terrain_at_pass(Terrain#rf_map_tree.sw, X, Y, From);
get_terrain_from_children(X, Y, From, #rf_map_segment{x=StartX, y=StartY, width=Width, height=Height, terrain = Terrain}) when X - StartX >= Width div 2, Y - StartY >= Height div 2 ->
  terrain_at_pass(Terrain#rf_map_tree.se, X, Y, From).
  

split_segment(#rf_map_segment{x=X, y=Y, width=Width, height=Height, terrain=Terrain}) ->
  {NW, NE, SW, SE} = split_terrain(Terrain, Width, Height),
  SegWidth = Width div 2,
  SegHeight = Height div 2,
  NWSegment = #rf_map_segment{x=X, y=Y, width=SegWidth, height = SegHeight, terrain=NW},
  NESegment = #rf_map_segment{x=X + SegWidth, y=Y, width=SegWidth, height = SegHeight, terrain=NE},
  SWSegment = #rf_map_segment{x=X, y=Y+SegHeight, width=SegWidth, height = SegHeight, terrain=SW},
  SESegment = #rf_map_segment{x=X+SegWidth,y=Y+SegHeight,width=SegWidth,height=SegHeight, terrain=SE},
  {NWSegment, NESegment, SWSegment, SESegment}.

split_terrain(Terrain, Width, Height) ->
  {North, South} = lists:split(Height  div  2, Terrain),
  {NW, NE} = split_west_east(North, Width),
  {SW, SE} = split_west_east(South, Width),
  {NW, NE, SW, SE}.

split_west_east(Terrain, Width) ->
  split_west_east(Terrain, Width, {[],[]}).

split_west_east([], _Width, {West, East}) ->
  {lists:reverse(West), lists:reverse(East)};
split_west_east([CurTerrain| Terrain], Width, {West, East}) ->
  {WestHalf, EastHalf} = lists:split(Width div 2, CurTerrain),
  split_west_east(Terrain, Width, {[WestHalf | West], [EastHalf | East]}).
  
