-module(test).
-export([start/1, connect/5, disconnect/4, update/1, broadcast/1]).

start(Node) ->
    % Create routers
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, uppsala),
    routy:start(r4, gothenburg),
    routy:start(r5, kiruna),

    % Set Routers to list of routers for easier broadcast and update.
    Routers = [r1, r2, r3, r4, r5],

    % Connect the routers as: r4 <-> r1 <-> r2 <-> r3 <-> r5
    connect(r4, gothenburg, r1, stockholm, Node),
    connect(r1, stockholm, r2, lund, Node),
    connect(r2, lund, r3, uppsala, Node),
    connect(r3, uppsala, r5, kiruna, Node),
    
    % Send a broadcast from all routers
    broadcast(Routers),

    % Wait a while to make sure every router has received the broadcasts
    timer:sleep(100),

    % Update all the routers' routing tables
    update(Routers),

    % Print router map in shell
    io:format("Map: (r4, gothenburg) <-> (r1, stockholm) <-> (r2, lund) <-> (r3, uppsala) <-> (r5, kiruna)~n"),
    
    % Wait, for ease of seeing result
    timer:sleep(2000),

    % Send a message from Kiruna (r5) to Gothenburg (r4) (end-node to end-node)
    io:format("Sending a message from Kiruna (r5) to Gothenburg (r4)...~n"),
    timer:sleep(1000),
    r5 ! {send, gothenburg, hello_gothenburg_from_kiruna},

    % Pause
    timer:sleep(3000),

    % Send a message from Gothenburg (r4) to Kiruna (r5) (end-node to end-node)
    io:format("~nSending a message from Gothenburg (r4) to Kiruna (r5)...~n"),
    timer:sleep(1000),
    r5 ! {send, gothenburg, hello_kiruna_from_gothenburg},

    % Pause
    timer:sleep(3000),

    % Disconnect between Gothenburg and Stockholm
    io:format("~nDisconnect Gothenburg (r4) and Stockholm (r1)~n"),
    disconnect(r1, stockholm, r4, gothenburg),
    timer:sleep(50),
    broadcast(Routers),
    timer:sleep(50),
    update(Routers),
    timer:sleep(50),
    r4 ! status,
    timer:sleep(50),
    r1 ! status,

    % Pause 
    timer:sleep(3000),

    io:format("Sending a message from Kiruna (r5) to Gothenburg (r4)...~n"),
    timer:sleep(1000),
    r5 ! {send, gothenburg, hello_gothenburg_from_kiruna},

    % Pause
    timer:sleep(1000),

    io:format("~nSending a message from Gothenburg (r4) to Kiruna (r5)...~n"),
    timer:sleep(1000),
    r5 ! {send, gothenburg, hello_kiruna_from_gothenburg},

    % Pause
    timer:sleep(3000).

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
