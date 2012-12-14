-module(red_fields_instance).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  gen_server:start_link(?MODULE, {fake, fake}, []).

init({MapData, Players}) ->
  {ok, MapData}.
