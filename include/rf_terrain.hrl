-record(rf_terrain, {elevation=0, type=none}).
-record(rf_map_segment, {area, terrain=[]}).
-record(rf_terrain_type, {id, speed_mult = 1}).

-record(rf_unit_type, {id}).
