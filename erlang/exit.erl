-module(exit).
-export([on_exit/2]).

on_exit(Pid, Fun) ->
    spawn(fun() ->
                  process_flag(trap_exit, true),
                  link(Pid),
                  receive
                      {'EXIT', Pid, Why} ->
                          Fun(Why)
                  end
          end).


% F = fun() -> receive X -> list_to_atom(X) end end.
% Pid = spawn(F).
% %exit:on_exit(Pid, fun(Why) -> io:format(" ~p died with:~p~n", [Pid, Why]) end).
% Pid ! hello.
