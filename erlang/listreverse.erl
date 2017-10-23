-module(listreverse).
-export([reverse/1]).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Rlist) ->
    reverse(Rest, [Head | Rlist]);
reverse([], Rlist) ->
    Rlist.



