-module(rf_actor_locator).

-behavior(gen_server).
-include("include/rf_locator.hrl").
-include("include/rf_map_tree.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3]).
-export([update_sim/2, locate_actors/2]).

start_link(Sim) ->
  gen_server:start_link(?MODULE, Sim, []).

init(Sim) ->
  {ok, #rf_actor_locator{sim=Sim}}.

handle_call({update_sim, NewSim}, _Tag, Locator) ->
  {reply, ok, Locator#rf_actor_locator{sim=NewSim}};
handle_call({locate_actors, Filters}, _Tag, Locator) ->
  Actors = find_actors(Filters, Locator),
  {reply, Actors, Locator}.

update_sim(Pid, NewSim) ->
  gen_server:call(Pid, {update_sim, NewSim}).

locate_actors(Pid, Filters) ->
  gen_server:call(Pid, {locate_actors, Filters}).

find_actors(Filters, Locator) ->
  find_actors(Filters, Locator, []).

find_actors([], _Locator, Actors) ->
  Actors;
find_actors([Filter | Filters], #rf_actor_locator{sim=Sim} = Locator, Actors) ->
  NewActors = process_filter(Filter, Sim),
  find_actors(Filters, Locator, NewActors ++ Actors).

process_filter(#rf_actor_filter{area=Area}=Filter, Sim) ->
  SimArea = rf_simulation:get_area(Sim),
  find_parent_or_descend_tree(rf_area:contains(SimArea, Area), Filter, Sim).

find_parent_or_descend_tree(true, Filter, Sim) ->
  descend_tree(Filter, Sim);
find_parent_or_descend_tree(false, Filter, Sim) ->
  Parent = rf_simulation:get_parent(Sim),
  process_filter(Filter, Parent).


descend_tree(Filter, Sim) ->
  Children = rf_simulation:get_children(Sim),
  apply_filter_or_descend_tree(Children, Filter, Sim).

apply_filter_or_descend_tree(none, Filter, Sim) ->
  rf_simulation:filter_actors(Sim, Filter);
apply_filter_or_descend_tree(#rf_map_tree{}=Tree, Filter, _Sim) ->
  lists:foldl(fun(Child, Acc) -> [descend_tree(Filter, Child) | Acc] end, [], rf_map_tree:to_list(Tree)).
