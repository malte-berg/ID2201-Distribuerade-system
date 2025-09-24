-module(loggy).

-export([start/1, stop/1, print_safe/2]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Nodes, Clock, []).

loop(Nodes, Clock, Holdback) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedHoldback = [{From, Time, Msg} | Holdback],
            print_safe(UpdatedClock, UpdatedHoldback),
            loop(Nodes, Clock, Holdback);
        stop ->
            ok
    end.

print_safe(Clock, Holdback) ->
    ListOfSafe = [S || S = {_From, Time, _Msg} <:- Holdback, time:safe(Time, Clock)],
    SortedListOfSafe = lists:keysort(2, ListOfSafe),
    lists:foreach(fun({From, Time, Msg}) -> log(From, Time, Msg) end, SortedListOfSafe).

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

