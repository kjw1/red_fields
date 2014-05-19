-module(rf_simulation).
-include("include/rf_simulation.hrl").
-include("include/rf_game.hrl").
-behavior(gen_server).
-export([filter_actors/2, notify/2, transfer_actor/2, simulate_time_step/3, get_neighbor/3]).
-export([get_area/1,get_parent/1]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).

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

simulate_time_step(Pid, OriginPid, Ref) ->
  gen_server:cast(Pid, {simulate_time_step, OriginPid, Ref}).

get_neighbor(Pid, X, Y) ->
  gen_server:call(Pid, {find_sim_by_coord, X, Y}).

get_parent(Pid) ->
  gen_server:call(Pid, get_parent).

get_area(Pid) ->
  gen_server:call(Pid, get_area).

simulate_actor(Actor, Self) ->
  Context = generate_context(Actor, Self),
  rf_actor:simulate_time(Context, Actor).

%% TODO Get the current state the actor is aware of
%% such as nearby actors, terrain, and other info
generate_context(_Actor, _Self) ->
  {}.

handle_cast({simulate_time_step, OriginPid, Ref}, #rf_simulation{actors=Actors}=Self) ->
  Actions = lists:foldl(fun(Actor, Actions) ->
                          simulate_actor(Actor, Self) ++ Actions
                        end, Actors, []),
  UpdatedSelf = process_actions(Actions, Self),
  OriginPid ! { finished_sim, Ref },
  {noreply, UpdatedSelf}.

process_actions([], Self) ->
  Self;
process_actions([Action | Actions], Self ) ->
  io:format("Processing action: ~p~n", [Action]),
  process_actions(Actions, Self).
  
handle_call(get_area, _tag, #rf_simulation{area=Area}=Self) ->
  {reply, Area, Self};
handle_call({filter_actors, Filter}, _Tag, Self) ->
  Actors = do_filter_actors(Filter, Self),
  {reply, Actors, Self}.

do_filter_actors(Filter, #rf_simulation{actors=Actors}) ->
  lists:filter(fun(#rf_actor{}=Actor) -> Filter(Actor) end, Actors).
  
