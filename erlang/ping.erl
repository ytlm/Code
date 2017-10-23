-module(ping).
-export([startPing/1, startPong/0, ping/2, pong/0]).

ping(0, PongNode) ->
    {pong, PongNode} ! finished,
    io:format("ping finished~n", []);
ping(N, PongNode) ->
    {pong, PongNode} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong ~n", [])
    end,
    ping(N - 1, PongNode).

pong() -> receive
              finished ->
                  io:format("pong finished~n", []);
              {ping, PingPid} ->
                  io:format("Pong received ping ~n", []),
                  PingPid ! pong,
                  pong()
          end.

startPing(PongNode) ->
    spawn(ping, ping, [3, PongNode]).

startPong() ->
    register(pong, spawn(ping, pong, [])).
