-module(key).

-export([generate/0, between/3]).

generate() ->
    random:uniform(1_000_000_000).

between(_Key, N, N) ->
    true;
between(Key, From, To) ->
    case From < To of
        true ->
            From < Key andalso Key =< To;
        _ ->
            From < Key orelse Key =< To
    end.

