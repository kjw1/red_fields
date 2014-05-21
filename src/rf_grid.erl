-module(rf_grid).

-export([get_chunk/5, get_middle/3]).

get_middle(Start, Length, List) ->
  {Head, LongReplace} = lists:split(Start-1, List),
  {Replace, Tail} = lists:split(Length, LongReplace),
  {Head, Replace, Tail}.

get_chunk(X, Y, Width, Height, Map) ->
  {_, Rows, _} = get_middle(Y, Height, Map),
  lists:foldl(fun(Row, Acc) ->
        {_, SubRow, _} = get_middle(X, Width, Row),
        [SubRow | Acc] end, [ ], Rows).
