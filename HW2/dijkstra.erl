-module(dijkstra).
-export([entry/2, replace/4]).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, Length, _} -> % We are only interested in the length of the path
            Length;
        false ->
            0
    end.

replace(_Node, _N, _Gateway, _Sorted) ->
    ok.
