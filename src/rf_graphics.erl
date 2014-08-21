-module(rf_graphics).

-include_lib("wx/include/wx.hrl").

-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

-record(state, { frame, wx, box_pos={30, 30}, delta={0, 0}, sprites} ).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, 42, "Example", [{size, {800, 600}}]),
  wxFrame:show(Frame),
  Wx = rf_wx:start([{parent, Frame}, {controller, self()}]),
  erlang:send_after(500, self(), redraw),
  { ok, SpriteMapping } = application:get_env(sprite_map),
  SpriteDict = lists:foldl(fun({Id, Sprite}, Dict) -> 
         dict:store(Id, Sprite, Dict)
    end, dict:new(), SpriteMapping),
  {ok, #state{frame=Frame, wx=Wx, sprites = SpriteDict}}.


handle_info({click, X, Y}, #state{box_pos = {PX, PY}} = State) ->
  NewDelta = normalize({X - PX, Y - PY}),
  io:format("Got delta: ~p~n", [NewDelta]),
  {noreply, State#state{delta = NewDelta}};
handle_info(redraw, #state{wx=Wx, box_pos=Box, delta=Delta, sprites=Sprites}=State) ->
  TerrainDraws = create_terrain_draws(Sprites),
  UnitDraws = create_unit_draws(Sprites),
  %io:format("Drawing: ~p~n", [TerrainDraws]),
  wx_object:get_pid(Wx) ! {redraw, TerrainDraws ++ UnitDraws},
  erlang:send_after(20, self(), redraw),
  NewPos = add_delta(Box, vector_to_speed(Delta, 3)),
  %io:format("New Position: ~p~n", [NewPos]),
  {noreply, State#state{box_pos=NewPos}}.

add_delta({X, Y}, {DX, DY}) ->
  {X + DX, Y+DY}.

normalize({X, Y}) ->
  Dist = math:sqrt(math:pow(X, 2) + math:pow(Y, 2)),
  {X / Dist, Y / Dist}.
  
vector_to_speed({UnitX, UnitY}, Speed) ->
  {round(UnitX * Speed), round(UnitY * Speed)}.

terminate(_Reason, _State) ->
  ok.

create_unit_draws(Sprites) ->
  ets:foldl(fun(Actor, Draws) ->
        Type = rf_actor:get_type(Actor),
        {X, Y} = rf_actor:get_pos(Actor),
        Sprite = dict:fetch(Type, Sprites),
        [ {draw_sprite, Sprite, 1, X, Y} | Draws ]
    end, [], actors).


create_terrain_draws(Sprites) ->
  {Width, Height} = rf_map:get_meta(size),
  %io:format("Got map size: ~p ~p~n", [Width, Height]),
  Map = rf_map:get_terrain(1, 1, Width, Height),
  create_map_draws(Map, Sprites).

create_map_draws(Map, Sprites) ->
  {DrawCommands, _RowNum} = lists:foldl(fun(Row, {Rows, RowNum}) -> 
        { [ create_row_draws(Row, RowNum, Sprites) | Rows], RowNum + 1}
      end, {[], 0}, Map),
  lists:flatten(DrawCommands).

create_row_draws(Row, RowNum, Sprites) ->
  {RowCommands, _ColNum} = lists:foldl(fun(TerrainType, {Squares, ColNum}) ->
          TerrainSprite = dict:fetch(TerrainType, Sprites),
          Square = {draw_sprite, TerrainSprite, 1, ColNum * 16, RowNum * 16},
          { [Square | Squares], ColNum + 1}
      end, {[], 0}, Row),
  RowCommands.



