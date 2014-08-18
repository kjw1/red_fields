-module(rf_graphics).

-include_lib("wx/include/wx.hrl").

-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

-record(state, { frame, wx, box_pos={30, 30}, delta={0, 0}} ).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, 42, "Example", [{size, {800, 600}}]),
  wxFrame:show(Frame),
  Wx = rf_wx:start([{parent, Frame}, {controller, self()}]),
  erlang:send_after(500, self(), redraw),
  {ok, #state{frame=Frame, wx=Wx}}.


handle_info({click, X, Y}, #state{box_pos = {PX, PY}} = State) ->
  NewDelta = normalize({X - PX, Y - PY}),
  io:format("Got delta: ~p~n", [NewDelta]),
  {noreply, State#state{delta = NewDelta}};
handle_info(redraw, #state{wx=Wx, box_pos=Box, delta=Delta}=State) ->
  TerrainDraws = create_terrain_draws(),
  %io:format("Drawing: ~p~n", [TerrainDraws]),
  wx_object:get_pid(Wx) ! {redraw, TerrainDraws},
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

create_terrain_draws() ->
  {Width, Height} = rf_map:get_meta(size),
  %io:format("Got map size: ~p ~p~n", [Width, Height]),
  Map = rf_map:get_terrain(1, 1, Width, Height),
  create_map_draws(Map).

create_map_draws(Map) ->
  {DrawCommands, _RowNum} = lists:foldl(fun(Row, {Rows, RowNum}) -> 
        { [ create_row_draws(Row, RowNum) | Rows], RowNum + 1}
      end, {[], 0}, Map),
  lists:flatten(DrawCommands).

create_row_draws(Row, RowNum) ->
  {RowCommands, _ColNum} = lists:foldl(fun(_TerrainType, {Squares, ColNum}) ->
        Square = {draw_sprite, grass, 1, ColNum * 16, RowNum * 16},
        { [Square | Squares], ColNum + 1}
      end, {[], 0}, Row),
  RowCommands.



