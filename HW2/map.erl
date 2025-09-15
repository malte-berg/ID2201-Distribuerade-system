-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, []) -> % If empty map
    [{Node, Links}];
update(_Node, _Links, _Map) -> % If map has entries
    ok.

reachable(_, _) ->
    ok.

all_nodes(_) ->
    ok.
