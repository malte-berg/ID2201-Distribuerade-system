-module(node1).

-define(Stabilize, 1000).

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor)
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            {_, Ppid} = Pred,
            Ppid ! {notify, {Id, self()}};
        {Id, _} ->
            ok;
        {Skey, _} ->
            {_, Ppid} = Pred,
            Ppid ! {notify, {Id, self()}};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {notify, {Id, self()}},
                    stabilize(Pred, Id, {Xkey, Xpid});
                false ->
                    Spid ! {notify, {Id, self()}}
            end
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            ok;
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    ok;
                false ->
                    ok
            end
    end.

