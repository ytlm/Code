-module(udp).
-export([start_server/0, client/1]).

start_server() ->
    spawn(fun() -> server(4000) end).


server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opened socket: ~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("server received : ~p~n", [Msg]),
            {Ref, N} = binary_to_term(Bin),
            Fac = fac(N),
            gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
            loop(Socket)
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).



client(Request) ->
    {ok, Socket} = gen_udp:open(0, [binary]),

    Ref = make_ref(),
    Req = term_to_binary({Ref, Request}),

    io:format("client opened socket = ~p~n", [Socket]),

    ok = gen_udp:send(Socket, "localhost", 4000, Req),

    Value = wait_for_ref(Socket, Ref),

    gen_udp:close(Socket),

    Value.

wait_for_ref(Socket, Ref) ->
    receive
        {udp, Socket, _, _, Bin} = Msg ->
            io:format("client received : ~p~n", [Msg]),
            case binary_to_term(Bin) of
                {Ref, Val} ->
                    Val;
                {_SomeOtherRef, _} ->
                    wait_for_ref(Socket, Ref)
            end
    after 1000 ->
              0
    end.

