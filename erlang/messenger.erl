-module(messenger).

-export([startServer/0, server/1, login/0, logout/0, message/2, client/2]).

serverNode() ->
    messenger@centos.

localNode() ->
    node().

server(UserList) ->
    receive
        {From, logout} ->
            NewUserList = serverLogout(From, UserList),
            io:format("list is now: ~p~n", [NewUserList]),
            server(NewUserList);
        {From, login, Name} ->
            NewUserList = serverLogin(From, Name, UserList),
            io:format("list is now: ~p~n", [NewUserList]),
            server(NewUserList);
        {From, message, ToName, Message} ->
            serverTransfer(From, ToName, Message, UserList),
            server(UserList)
    end.

startServer() ->
    register(messenger, spawn(messenger, server, [[]])).

serverLogin(From, Name, UserList) ->
    case lists:keymember(Name, 2, UserList) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            UserList;
        false ->
            From ! {messenger, logged_in},
            [{From, Name} | UserList]
    end.

serverLogout(From, UserList) ->
    lists:keydelete(From, 1, UserList).

serverTransfer(From, ToName, Message, UserList) ->
    case lists:keysearch(From, 1, UserList) of
        false ->
            From ! {messenger, stop, you_are_not_logged_in};
        {value, {From, Name}} ->
            serverTransfer(From, Name, ToName, Message, UserList)
    end.

serverTransfer(From, Name, ToName, Message, UserList) ->
    case lists:keysearch(ToName, 2, UserList) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, ToName}} ->
            ToPid ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.

login() ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client, spawn(messenger, client, [serverNode(), localNode()]));
        _ -> already_logged_in
    end.

logout() ->
    case whereis(mess_client) of
        undefined ->
            not_logged_in;
        _ ->
            mess_client ! logout
    end.

message(ToName, Message) ->
    case whereis(mess_client) of
        undefined ->
            not_logged_in;
        _ ->
            mess_client ! {message_to, ToName, Message},
            ok
    end.

client(ServerNode, Name) ->
    {messenger, ServerNode} ! {self(), login, Name},
    awaitResult(),
    client(ServerNode).

client(ServerNode) ->
    receive
        logout ->
            {messenger, ServerNode} ! {self(), logout},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, ServerNode} ! {self(), message, ToName, Message},
            awaitResult();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(ServerNode).

awaitResult() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->
            io:format("~p~n", [What])
    end.

