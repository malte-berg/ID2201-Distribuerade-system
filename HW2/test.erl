-module(test).
-export([start/1, connect/5, disconnect/4, update/1, broadcast/1]).

start(Node) ->
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, uppsala),
    routy:start(r4, gothenburg),
    routy:start(r5, kiruna),
    Routers = [r1, r2, r3, r4, r5],
    connect(r4, gothenburg, r1, stockholm, Node),
    connect(r1, stockholm, r2, lund, Node),
    connect(r2, lund, r3, uppsala, Node),
    connect(r3, uppsala, r5, kiruna, Node),
    broadcast(Routers),
    timer:sleep(100),
    update(Routers),
    io:format("Map: (r4, gothenburg) <-> (r1, stockholm) <-> (r2, lund) <-> (r3, uppsala) <-> (r5, kiruna)").

connect(R1, Name1, R2, Name2, Node) ->
    R1 ! {add, Name2, {R2, Node}},
    R2 ! {add, Name1, {R1, Node}}.

disconnect(R1, Name1, R2, Name2) ->
    R1 ! {remove, Name2},
    R2 ! {remove, Name1}.
   
update(Routers) ->
    lists:foreach(fun(R) -> R ! update end, Routers).

broadcast(Routers) ->
    lists:foreach(fun(R) -> R ! broadcast end, Routers).
