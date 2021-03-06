
-module(red_fields_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Units, MapSize) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Units, MapSize]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Units, MapSize]) ->
    {ok, { {one_for_one, 5, 10}, [
        {rf_simulation, {rf_simulation, start_link, [Units, MapSize, none]},
         permanent, 5000, worker, [rf_simulation]},
      ?CHILD(rf_graphics, worker)]} }.

