-record(rf_game, { waiting_actors, finished_actors, clock }).
-record(rf_actor, { id, x, y, plist=[] }).
-record(rf_physical, { position, velocity }).
                   
