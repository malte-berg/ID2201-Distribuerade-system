-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    P = fun({Key, _}) -> key:between(Key, From, To) end,
    lists:partition(P, Store).

merge(Entries, Store) ->
    Entries ++ Store.

