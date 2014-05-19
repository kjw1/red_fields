-module(rf_graphics).

-include_lib("wx/include/wx.hrl").

-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

-record(state, { frame, wx, box_pos={30, 30}} ).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, 42, "Example", [{size, {800, 600}}]),
  wxFrame:show(Frame),
  Wx = rf_wx:start([{parent, Frame}]),
  erlang:send_after(500, self(), redraw),
  {ok, #state{frame=Frame, wx=Wx}}.


handle_info(redraw, #state{wx=Wx, box_pos={X, Y}=Box}=State) ->
  wx_object:get_pid(Wx) ! {redraw, Box},
  erlang:send_after(50, self(), redraw),
  {noreply, State#state{box_pos={X + 1, Y}}}.
  

terminate(_Reason, _State) ->
  ok.

