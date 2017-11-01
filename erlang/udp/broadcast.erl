-module(broadcast).

-export([send/1, listen/0]).

send(IoList) ->
    case inet:ifget("enp0s25", [broadcast]) of
        {ok, [{broadcast, Ip}]} ->
            {ok, S} = gen_udp:open(5010, [{broadcast, true}]),
            gen_udp:send(S, Ip, 6000, IoList),
            gen_udp:close(S);
        _Any ->
            io:format("Bad interface name, or broadcasting not suppored\n"
                     "error : ~p~n", [_Any])
    end.

listen() ->
    {ok, S} = gen_udp:open(6000),
    loop(S).

loop(S) ->
    receive
        Any ->
            io:format("received : ~p~n", [Any]),
            loop(S)
    end.
