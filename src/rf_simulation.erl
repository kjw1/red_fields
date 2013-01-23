-module(rf_simulation).
-include("include/rf_simulation.hrl").
-behavior(gen_server).
-export([start_link/2]).
-export([init/1]).

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

pass_filter_up(Pid, ActorFilter) ->
  gen_server:cast(Pid, {pass_filter, ActorFilter}).


handle_call({pass_filter, #rf_actor_filter{area=Area} = ActorFilter}, #rf_simulation{area=SimArea} = Sim) ->
  pass_or_handle(contains(SimArea, Area), ActorFilter, Sim).

pass_or_handle(true, ActorFilter, #rf_simulation{children=Children}) ->
  CList = map_tree:to_list(Children),
  lists:foreach(fun(Child) -> filter_actors(Child, ActorFilter) end, CList);
pass_or_handle(false, ActorFilter, #rf_simulation{parent=Parent}) ->
  pass_filter_up(Parent, ActorFilter).
