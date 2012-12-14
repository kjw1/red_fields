
-module(rf_actor).
-behavior(gen_server).
-export([init/1, start_link/1]).

start_link(Clock) ->
  gen_server:start_link(?MODULE, [Clock], []).

simulate(Pid, Game, Time) ->
  gen_server:cast(Pid, {simulate, Game, Time}).

init(Args) ->
  {ok, Args}.

handle_cast({simulate, Game, Time}, State) ->
  {reply, ok, State}.
