-module(red_fields).
-include("include/rf_game.hrl").
-export([new_game/0, simulate_response_message/2]).

new_game() ->
  #rf_game{actors=dict:new(), clock = 0}.

setup_map_segments(MapDefinition) ->
  

simulate_actors([], Game) ->
  Game;
simulate_actors([{_Key, Actor} | Actors] , Game) ->
  rf_actor:simulate(Actor, Game),
  simulate_responses(Game).

simulate_response([], Game) ->
  Game;
simulate_response([Event | Events], Game) ->
  NextGame = case Event of
    {create, Type, Params} ->
      %TODO implement this
      Game;
    {remove_actor, Id} ->
      %TODO implement this
      Game;
    _ ->
      Game
  end,
  simulate_response(Events, NextGame).

simulate_response_message(Game, {actor_update, UpdateList}) ->
  simulate_response(UpdateList, Game).
    
