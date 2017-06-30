-module(motor_controller).
-import(event_handler, [make/1, add_handler/2, event/2]).
-export([add_event_handler/0]).

add_event_handler() ->
    event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
    io:format("Turn off the motor~n");
controller(X) ->
    io:format("~w ignored event: ~p~n", [?MODULE, X]).


