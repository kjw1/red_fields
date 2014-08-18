-module(test_utils).

-export([make_map/2]).

make_map(Width, Height) ->
  Ys = lists:seq(1, Height),
  Xs = lists:seq(1, Width),
  lists:map(fun(Y) -> lists:map(fun(X) -> {X, Y} end, Xs) end, Ys).
