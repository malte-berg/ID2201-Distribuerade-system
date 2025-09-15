-module(hist).
-export([new/1, update/3]).

new(Name) ->
    [{Name, inf}].

update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {Node, Counter} ->
            if 
                N > Counter ->
                    Updated = lists:keyreplace(Node, 1, History, {Node, N}),
                    {new, Updated};
                true ->
                    old
            end;
        _ ->
            Updated = [{Node, N} | History],
            {new, Updated}
    end.

