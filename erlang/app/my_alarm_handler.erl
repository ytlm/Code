-module(my_alarm_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2,
         handle_call/2, handle_info/2,
         terminate/2]).

init(Args) ->
    io:format(" *** my alarm handler init : ~p~n", [Args]),
    {ok, 0}.

handle_event({set_alarm, tooHot}, N) ->
    error_logger:error_msg(" *** tell the enginer to turn on the fan~n"),
    {ok, N+1};

handle_event({clear_alarm, tooHot}, N) ->
    error_logger:error_msg(" *** danger over, turn off the fan~n"),
    {ok, N};

handle_event(Event, N) ->
    io:format(" *** unmatched event : ~p~n", [Event]),
    {ok, N}.

handle_call(_Request, N) -> Reply = N, {ok, N, N}.

handle_info(_Info, N) -> {ok, N}.

terminate(_Reason, N) -> ok.

%%
%% alarm_handler:set_alarm(tooHot).
%% gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {my_alarm_handler, xyz}).
%% alarm_handler:set_alarm(tooHot).
%% alarm_handler:clear_alarm(tooHot).
%%
