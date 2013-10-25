-module(rf_simulation).
-include("include/rf_simulation.hrl").
-include("include/rf_game.hrl").
-behavior(gen_server).
-export([filter_actors/2, notify/2, transfer_actor/2, simulate_time_step/2, get_neighbor/3]).
-export([get_area/1,get_parent/1]).
-export([start_link/2]).
-export([init/1, handle_call/3]).

start_link(Area, Parent) ->
  gen_server:start_link(?MODULE, {Area, Parent}, []).

init({Area, Parent}) ->
  {ok,#rf_simulation{area=Area, parent=Parent}}.

filter_actors(Pid, ActorFilter) ->
  gen_server:call(Pid, {filter_actors, ActorFilter}).

notify(Pid, Event) ->
  gen_server:cast(Pid, {event_notify, Event}).

transfer_actor(Pid, Actor) ->
  gen_server:cast(Pid, {transfer_actor, Actor}).

simulate_time_step(Pid, OriginPid) ->
  gen_server:cast(Pid, {simulate_time_step, OriginPid}).

get_neighbor(Pid, X, Y) ->
  gen_server:call(Pid, {find_sim_by_coord, X, Y}).

get_parent(Pid) ->
  gen_server:call(Pid, get_parent).

get_area(Pid) ->
  gen_server:call(Pid, get_area).



handle_call(get_area, _tag, #rf_simulation{area=Area}=Self) ->
  {reply, Area, Self};
handle_call({filter_actors, Filter}, _Tag, Self) ->
  Actors = do_filter_actors(Filter, Self),
  {reply, Actors, Self}.


do_filter_actors(Filter, #rf_simulation{actors=Actors}) ->
  lists:filter(fun(#rf_actor{}=Actor) -> Filter(Actor) end, Actors).
  
