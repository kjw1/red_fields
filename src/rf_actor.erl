-module(rf_actor).

-export([simulate_time/2, def_to_actor/1]).
-export([get_id/1,
         get_type/1,
         get_pos/1,
         get_dir/1
        ]).

-record(rf_actor, {id, type, pos, dir}).

%% Should return a list of actions to apply to other actors next time
simulate_time(Context, Actor) ->
  io:format("Simulating for: ~p with context: ~p ~n", [Actor, Context]),
  [].

get_type(#rf_actor{type=Type}) ->
  Type.
get_id(#rf_actor{id=Id}) ->
  Id.
get_pos(#rf_actor{pos=Pos}) ->
  Pos.
get_dir(#rf_actor{dir=Dir}) ->
  Dir.

def_to_actor({UnitType, X, Y}) ->
  #rf_actor{id = {os:timestamp(), self()}, type=UnitType, pos={X, Y}, dir = {0, 1}}.
