-module(dijkstra).
-export([entry/2, replace/4, update/4]).

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

% Thanks to entry returning 0 if node not in list, case will be false if node does not exist, meaning no need for an extra check.
% It also catches if an empty list is provided.
update(Node, N, Gateway, Sorted) ->
    case N < entry(Node, Sorted) of
        true ->
            replace(Node, N, Gateway, Sorted);
        _ ->
            Sorted
    end.

