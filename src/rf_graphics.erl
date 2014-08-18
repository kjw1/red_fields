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
  SpriteData = rf_sprite:load_sprites(),
  {ok, #state{frame=Frame, wx=Wx, sprites=SpriteData}}.


handle_info({click, X, Y}, #state{box_pos = {PX, PY}} = State) ->
  NewDelta = normalize({X - PX, Y - PY}),
  io:format("Got delta: ~p~n", [NewDelta]),
  {noreply, State#state{delta = NewDelta}};
handle_info(redraw, #state{wx=Wx, box_pos=Box, delta=Delta}=State) ->
  wx_object:get_pid(Wx) ! {redraw, Box},
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



