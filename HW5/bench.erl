-module(bench).

-compile(export_all).

one(N1, NoOfKeys) ->
    Keys4K = test:keys(NoOfKeys),
    test:add(Keys4K, N1),
    Start = erlang:system_time(micro_seconds),
    test:check(Keys4K, N1),
    Finish = erlang:system_time(micro_seconds),
    io:format("~w~n", [Finish - Start]).

four(N1, NoOfKeys) ->
    K1 = test:keys(NoOfKeys div 4),
    K2 = test:keys(NoOfKeys div 4),
    K3 = test:keys(NoOfKeys div 4),
    K4 = test:keys(NoOfKeys div 4),
    Keys = [K1, K2, K3, K4],
    Nodes = [N1, N1, N1, N1],
    lists:foreach(fun(K) -> test:add(K, N1) end, Keys),
    Start = erlang:system_time(micro_seconds),
    spawn_runners(Keys, Nodes, 4, self()),
    await_finished(4),
    Finish = erlang:system_time(micro_seconds),
    io:format("~w~n", [Finish - Start]).

two_nodes(N1, N2) ->
    Keys = [test:keys(1000), test:keys(1000), test:keys(1000), test:keys(1000)],
    lists:foreach(fun(K) -> test:add(K, N1) end, Keys),
    Nodes = [N1, N2, N1, N2],
    timer:sleep(2000),
    Start = erlang:system_time(micro_seconds),
    spawn_runners(Keys, Nodes, 4, self()),
    await_finished(4),
    Finish = erlang:system_time(micro_seconds),
    io:format("~w~n", [Finish - Start]).

four_nodes(Nodes) ->
    Keys = [test:keys(1000), test:keys(1000), test:keys(1000), test:keys(1000)],
    [N1, _, _, _] = Nodes,
    lists:foreach(fun(K) -> test:add(K, N1) end, Keys),
    timer:sleep(2000),
    Start = erlang:system_time(micro_seconds),
    spawn_runners(Keys, Nodes, 4, self()),
    await_finished(4),
    Finish = erlang:system_time(micro_seconds),
    io:format("~w~n", [Finish - Start]).

spawn_runners(_, _, 0, _) ->
    ok;
spawn_runners([Keys | Rest], [Node | Nodes], Clients, Proc) ->
    spawn(fun() -> test:check(Keys, Node), Proc ! finished end),
    spawn_runners(Rest, Nodes, Clients-1, Proc).

await_finished(0) ->
    ok;
await_finished(Clients) ->
    receive
        finished ->
            await_finished(Clients-1)
    end.


