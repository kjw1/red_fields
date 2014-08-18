%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(rf_wx).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
   handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, 
  {
    controller,
    parent,
    config,
    canvas,
    sprites,
    bitmap
  }).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    io:format("Starting canvas~n"),
    Parent = proplists:get_value(parent, Config),  
    Controller = proplists:get_value(controller, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
         [{label, "Various shapes"}]),

    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    SpriteData = rf_sprite:load_sprites(),

    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Canvas, left_down),

    %% Add to sizers
    wxSizer:addSpacer(Sizer, 5),
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
        {proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
           {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),

    {W,H} = wxPanel:getSize(Canvas),
    Bitmap = wxBitmap:new(erlang:max(W,30),erlang:max(30,H)),
    
    {Panel, #state{parent=Panel, config=Config,
       controller = Controller, sprites= SpriteData,
       canvas = Canvas, bitmap = Bitmap}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
      #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxMouse{type = left_down, x=MouseX, y=MouseY}}, #state{controller=Controller}=State) ->
    io:format("Got mouse at ~p ~p~n", [MouseX, MouseY]),
    Controller ! {click, MouseX, MouseY},
    {noreply, State};
handle_event(#wx{event = #wxCommand{type = command_button_clicked}},
       State = #state{}) ->
    {noreply, State};
handle_event(#wx{event = #wxSize{size={W,H}}},
       State = #state{bitmap=Prev}) ->
    Bitmap = wxBitmap:new(W,H),
    draw(State#state.canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
    wxBitmap:destroy(Prev),
    {noreply, State#state{bitmap = Bitmap}};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info({redraw, Commands}, #state{canvas=Canvas, bitmap=Bitmap, sprites=SpriteData}=State) ->
    draw(Canvas, Bitmap, draw_sprites(Commands, SpriteData)),
    wxWindow:refresh(Canvas),
    {noreply, State};
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw_sprites(CommandList, SpriteData) ->
    fun(DC) ->
        wxDC:clear(DC),
        lists:foreach(fun({draw_sprite, SpriteId, FrameNum, X, Y}) ->
            Bitmap = rf_sprite:get_frame(SpriteId, FrameNum, SpriteData),
            wxDC:drawBitmap(DC, Bitmap, {X, Y})
          end, CommandList)
  end.


%% Buffered makes it all appear on the screen at the same time
draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    Fun(MemoryDC),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
        {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
        MemoryDC, {0,0}),    
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
        {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
        MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).

