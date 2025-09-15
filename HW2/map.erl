-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, []) -> % If empty map
    [{Node, Links}];
update(Node, Links, Map) -> % If map has entries
    lists:keyreplace(Node, 1, Map, {Node, Links}).

reachable(_, _) ->
    ok.

all_nodes(_) ->
    ok.
