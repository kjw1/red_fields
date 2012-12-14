-module(rf_simulation).
-behavior(gen_server).

start_link(X,Y, Width,Height) ->
  gen_server:start_link(?MODULE, {X, Y, Width, Height}, []).

init({X, Y, Width, Height}) ->
  {ok,#rf_simulation{x=X, y=Y, width=Width, height=Height}}.
