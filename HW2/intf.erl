-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, _, Pid} ->
            {ok, Pid};
        _ ->
            notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, Ref, _} ->
            {ok, Ref};
        _ ->
            notfound
    end.

