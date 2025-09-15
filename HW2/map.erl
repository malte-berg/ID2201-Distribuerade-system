-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, []) -> % If empty map
    [{Node, Links}];
update(Node, Links, Map) -> % If map has entries
    lists:keyreplace(Node, 1, Map, {Node, Links}).

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, Links} ->
            Links;
        false ->
            []
    end.

all_nodes(Map) ->
    lists:uniq(lists:flatten(lists:map(fun(Tuple) -> [element(1, Tuple)] ++ element(2, Tuple) end, Map))).
