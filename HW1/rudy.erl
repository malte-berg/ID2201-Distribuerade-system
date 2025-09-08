-module(rudy).
-export([init/1]).
% -- Task 2.2 Start --
-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").
% -- Task 2.2 End --

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            % -- Task 2.1 Start --
            handler(Listen),
            % -- Task 2.1 End --
            gen_tcp:close(Listen),
            ok;
        {error, _Error} ->
            error
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
        {error, _Error} ->
            error
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
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    % -- Task 2.1 Start -- 
    http:ok(URI).
    % -- Task 2.1 End --

