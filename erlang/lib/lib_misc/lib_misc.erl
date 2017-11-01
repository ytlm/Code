-module(lib_misc).
-export([string2value/1, sleep/1, random_seed/0,
		 dump/2]).

string2value(L) -> string2value(L, []).
string2value([], N) -> list_to_tuple(lists:reverse(N));
string2value([H|T], N) -> string2value(T, [H|N]).


sleep(T) ->
    receive
    after T ->
          true
    end.

random_seed() ->
    {_, _, X} = erlang:timestamp(),
    {H, M, S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1, M1, S1}).

dump(File, Term) ->
    io:format("** dumping to ~s~n",[File]),
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n",[Term]),
    file:close(S).
