-module(rf_actor).

-export([simulate_time/2]).

%% Should return a list of actions to apply to other actors next time
simulate_time(Context, Actor) ->
  io:format("Simulating for: ~p with context: ~p ~n", [Actor, Context]),
  [].
