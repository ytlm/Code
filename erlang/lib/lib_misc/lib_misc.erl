-module(lib_misc).

-import(lists, [all/2, any/2, filter/2, reverse/1, reverse/2,
                foreach/2, map/2, member/2, sort/1]).

-export([string2value/1, sleep/1, random_seed/0,
		 dump/2, trim/1]).

string2value(L) -> string2value(L, []).
string2value([], N) -> list_to_tuple(lists:reverse(N));
string2value([H|T], N) -> string2value(T, [H|N]).

trim(Bin) -> list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(X) -> reverse(skip_blanks_and_zero(reverse(X))).

skip_blanks_and_zero([$\s | T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0 | T])   -> skip_blanks_and_zero(T);
skip_blanks_and_zero(X)         -> X.

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
