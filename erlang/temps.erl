-module(temps).
-export([format/1]).

format([]) ->
    ok;
format([City | Rest]) ->
    print_temp(convert(City)),
    format(Rest).

convert({Name, {c, Temp}}) ->
    {Name, {c, Temp}};
convert({Name, {f, Temp}}) ->
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).

%% temps:format([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {pairs, {f, 28}}, {london, {f, 36}}]).
