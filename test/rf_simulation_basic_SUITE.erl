-module(rf_simulation_basic_SUITE).

-export([end_per_suite/1, init_per_suite/1, all/0]).

all() ->
  [].

init_per_suite(Config) ->
  application:start(red_fields),
  Config.

end_per_suite(_Config) ->
  application:stop(red_fields).
