-module(listmax).
-export([mmax/1]).

mmax([Head | Rest]) ->
    mmax(Head, Rest).

mmax(Result, []) ->
    Result;
mmax(Result, [Head|Rest])  when Head > Result ->
    mmax(Head, Rest);
mmax(Result, [Head|Rest]) ->
    mmax(Result, Rest).
