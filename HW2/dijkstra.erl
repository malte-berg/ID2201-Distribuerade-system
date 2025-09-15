-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2]).

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
            Sorted;
        _ ->
            ListToSort = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
            lists:keysort(2, ListToSort)
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

iterate([], _, Table) -> % If no more entries in list
    Table;
iterate([{_, inf, _}|_], _, Table) -> % If first entry in list is of inf length
    Table;
iterate(Sorted, Map, Table) ->
    [{Node, N, Gateway} | Tail] = Sorted,
    Links = map:reachable(Node, Map),
    UpdatedSorted = lists:foldl(fun(Link, Acc) -> update(Link, N+1, Node, Acc) end, Tail, Links), % N+1 as the link is 1 away from the original node
    iterate(UpdatedSorted, Map, [{Node, Gateway} | Table]).

table(Gateways, Map) ->
    SetDummy = fun(DummyNode, DummyAcc) -> [{DummyNode, inf, unknown} | DummyAcc] end,
    InitialList = lists:foldl(SetDummy, [], map:all_nodes(Map)),
    AddGateways = fun(GatewayToAdd, GatewayAcc) -> [{GatewayToAdd, 0, GatewayToAdd} | GatewayAcc] end,
    UpdatedList = lists:foldl(AddGateways, InitialList, Gateways),
    UpdatedList.

