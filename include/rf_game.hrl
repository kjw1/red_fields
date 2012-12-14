

-record(rf_game, { waiting_actors, finished_actors, clock }).
-record(rf_actor, { id, plist=[] }).
-record(rf_physical, { position, velocity }).
                   
