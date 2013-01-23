-module(rf_actor_locator).

-behavior(gen_server).
-export([start_link/1).
-export([init/1, handle_call/4]).
-export([register_filter/2, get_filtered_actors/1]).

-record(rf_actor_locator, {filters, sim}).
-record(rf_actor_filter, {ref, owner, area, filter}).

start_link(Sim) ->
  gen_server:start_link(?MODULE, {Sim}, []).

init({Sim}) ->
  {ok, #rf_actor_locator{sim=Sim, filters = dict:new()}}.
