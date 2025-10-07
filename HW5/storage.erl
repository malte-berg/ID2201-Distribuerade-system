-module(storage).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    P = fun({Key, _}) -> Key > From andalso Key =< To end,
    lists:partition(P, Store).

merge(Entries, Store) ->
    Entries ++ Store.

