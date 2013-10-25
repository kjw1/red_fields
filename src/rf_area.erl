-module(rf_area).
-include("include/rf_area.hrl").
-export([in_area/3, overlap/2, contains/2]).

in_area(#rf_area{x=X, y=Y, width=Width, height=Height}, PX, PY) when
    PX < X orelse PX > X + Width orelse
    PY < Y orelse PY > Y + Height ->
  false;
in_area(#rf_area{}, _PX, _PY) ->
  true.

overlap(#rf_area{x=X1, y=Y1, width=W1, height=H1}, #rf_area{x=X2, y=Y2, width=W2, height=H2}) when
    X1 + W1 < X2 orelse X1 > X2 + W2 orelse
    Y1 + H1 < Y2 orelse Y1 > Y2 + H2 ->
  false;
overlap(#rf_area{}, #rf_area{}) ->
  true.

contains(#rf_area{x=CX, y=CY, width=CW, height=CH}, #rf_area{x=X, y=Y, width=W, height=H}) when
    X >= CX andalso X + W =< CX + CW andalso
    Y >= CY andalso Y + H =< CY + CH ->
  true;
contains(#rf_area{}, #rf_area{}) ->
  false.
