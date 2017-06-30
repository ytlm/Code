-module(lib_misc).
-export([string2value/1]).

string2value(L) -> string2value(L, []).
string2value([], N) -> list_to_tuple(lists:reverse(N));
string2value([H|T], N) -> string2value(T, [H|N]).
