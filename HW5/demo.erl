-module(demo).

-compile(export_all).

construct_ring() ->
    N1 = node2:start(1000),
    N2 = node2:start(3000, N1),
    N3 = node2:start(5000, N1),
    [N1, N2, N3].

probe_all(Nodes) ->
    lists:foreach(fun(Node) -> Node ! probe, timer:sleep(50) end, Nodes).

store_all(Nodes) ->
    lists:foreach(fun(Node) -> Node ! store, timer:sleep(50) end, Nodes).

store_key(Id, Value, Node) ->
    test:add(Id, Value, Node).

