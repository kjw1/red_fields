-module(rf_grid).

-export([get_chunk/5, get_middle/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

safe_split(N, List) when N =< 0 ->
  {[], List};
safe_split(N, List) when N >= length(List)->
  {List, []};
safe_split(N, List) ->
  lists:split(N, List).

get_middle(Start, Length, List) ->
  %io:format("Getting middle ~p ~p ~p~n", [Start, Length, List]),
  {Head, LongReplace} = safe_split(Start-1, List),
  {Replace, Tail} = safe_split(Length + 1, LongReplace),
  {Head, Replace, Tail}.

get_chunk(X, Y, Width, Height, Map) when Width > 0 andalso Height > 0->
  {_, Rows, _} = get_middle(Y, Height - 1, Map),
  lists:reverse(
        lists:foldl(fun(Row, Acc) ->
            {_, SubRow, _} = get_middle(X, Width - 1, Row),
            [SubRow | Acc] end, [ ], Rows)).
