-module(rudy).
-export([init/2, start/2, stop/0]).

start(Port, Proc) ->
    register(rudy, spawn(fun() -> init(Port, Proc) end)).

stop() ->
    whereis(rudy) ! stop, ok;

init(Port, Proc) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler_pool(Listen, Proc),
            receive
                stop ->
                    gen_tcp:close(Listen)
            end,
            ok;
        {error, Error} ->
            io:format("rudy: init error: ~w~n", [Error])
    end.

handler_pool(_, 0) ->
    ok;
handler_pool(Listen, Proc) ->
    spawn(fun() -> handler(Listen) end),
    handler_pool(Listen, Proc-1).

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            io:format("rudy: handler error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: request error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).

