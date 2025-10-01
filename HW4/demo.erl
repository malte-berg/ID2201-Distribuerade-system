-module(demo).

-export([demo/0]).

demo() ->
    W1 = test:first(1, gms3, 1000),
    timer:sleep(50),
    W2 = test:add(2, gms3, W1, 1000),
    timer:sleep(50),
    loop(3, W2).

loop(N, Prev) ->
    timer:sleep(3000),
    New = test:add(N, gms3, Prev, 1000),
    loop(N + 1, New).
