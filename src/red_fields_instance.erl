-module(red_fields_instance).
-behavior(gen_server).
-export([init/1].

init({MapData, Players}) ->
  {ok, MapData}.
