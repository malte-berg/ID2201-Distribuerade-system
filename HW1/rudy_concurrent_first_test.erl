-module(rudy_con).
-export([init/1]).
% -- Task 2.2 Start --
-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    whereis(rudy) ! stop, ok.
% -- Task 2.2 End --

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            spawn(fun() -> handler(Listen) end),
            spawn(fun() -> handler(Listen) end),
            spawn(fun() -> handler(Listen) end),
            spawn(fun() -> handler(Listen) end),
            receive
                stop ->
                    gen_tcp:close(Listen)
            end;
        {error, Error} ->
            io:format("rudy: init error: ~w~n", [Error])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            % -- Task 2.1 Start --
            request(Client),
            % -- Task 2.2 Start --
            handler(Listen);
            % -- Task 2.2 End --
            % -- Task 2.1 End --
        {error, Error} ->
            io:format("rudy: handler error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            % -- Task 2.1 Start --
            Request = http:parse_request(Str),
            % -- Task 2.1 End --
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: request error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    % -- Task 3 Start --
    % timer:sleep(40),
    % -- Task 3 End --
    % -- Task 2.1 Start -- 
    http:ok(URI).
    % -- Task 2.1 End --

