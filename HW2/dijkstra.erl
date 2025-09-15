-module(dijkstra).
-export([entry/2, replace/4]).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, Length, _} -> % We are only interested in the length of the path
            Length;
        false ->
            0
    end.

replace(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 ->
            error;
        _ ->
            lists:sort(fun(A, B) -> entry(element(1,A), Sorted) > entry(element(1,B), Sorted) end,lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})) % > is used, reverse logic
    end.

