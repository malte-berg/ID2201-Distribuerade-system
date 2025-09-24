-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(Name, T) ->
    {Name, T+1}.

merge(Ti, Tj) ->
    max(Ti, Tj).

leq(Ti, Tj) ->
    Ti =< Tj.

clock(Nodes) ->
    lists:foldl(fun(Node, Clock) -> [{Node, zero()} | Clock] end, [], Nodes).

update(Node, Time, Clock) ->
    case lists:keyfind(Node, 1, Clock) of
        {Node, Lamport} ->
            case leq(Lamport, Time) of
                true ->
                    lists:keyreplace(Node, 1, Clock, {Node, Time});
                false ->
                    Clock
            end;
        false ->
            Clock
    end.

safe(Time, Clock) ->
    TimesList = lists:map(fun(C) -> element(2, C) end, Clock),
    Min = lists:min(TimesList),
    leq(Time, Min).

