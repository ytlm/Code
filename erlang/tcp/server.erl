-module(server).
-export([start_nano_server/0,
        start_seq_server/0,
        start_parallel_server/0]).

listen() ->
    gen_tcp:listen(2345, [binary, {packet, 4},
                          {reuseaddr, true},
                          {active, true}]).

%%
%% parallel server
%%
start_parallel_server() ->
    {ok, Listen} = listen(),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),

    loop(Socket).

%%
%% sequential server
%%
start_seq_server() ->
    {ok, Listen} = listen(),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

%%
%% simple one request server
%%
start_nano_server() ->
    {ok, Listen} = listen(),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).


loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            Reply = lib_misc:string2value(Str),
            io:format("Server replying = ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

