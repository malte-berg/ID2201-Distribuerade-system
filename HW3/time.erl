-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1]).

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

