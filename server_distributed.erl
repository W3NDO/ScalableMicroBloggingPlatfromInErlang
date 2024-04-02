-module(server_distributed).

-include_lib("eunit/include/eunit.hrl").

-export([initialize/1, initialize_with/2, server_actor/3, hash_users/2, initialize_test/1, typical_session_test/1, typical_session_1/2, typical_session_2/2]).

initialize(NumberOfServers) -> 
    initialize_with(dict:new(), NumberOfServers).

initialize_with(Users, NumberOfServers) ->
    % io:format("Spinning up ~w servers~n", [NumberOfServers]),
    ServersPids = spawn_servers(NumberOfServers, [], Users),
    ServersPids.


spawn_servers(0, ExistingServersPids, _Users) ->
    lists:map(fun(S) -> 
        S ! { self(), update_server_list, ExistingServersPids} 
    end, ExistingServersPids ),
    ExistingServersPids;

spawn_servers(N, ExistingServersPids, Users) ->
    NewServerPid = spawn(?MODULE, server_actor, [Users, ExistingServersPids, []]), % change this to just spawn
    ServerName = list_to_atom(io_lib:format("server_~B", [N])),
    catch unregister(ServerName),
    register(ServerName, NewServerPid),
    spawn_servers(N-1, [NewServerPid | ExistingServersPids], Users).

server_actor(Users, Servers, SubMessages) ->
    receive
        {Sender, server_info } -> 
            io:format("Users: ~p~n", [length(Users)]),
            io:format("Servers: ~p~n", [length(Servers)]),
            io:format("Messages ~p~n", [erlang:process_info(self(), message_queue_len)]),
            Sender ! { self(), server_info, [Users, Servers]},
            server_actor(Users, Servers, SubMessages);

        {Sender, show_servers } -> 
            io:format("Running Servers -> ~p~n", [Servers]),
            Sender ! {self(), Servers},
            server_actor(Users, Servers, SubMessages);

        {Sender, update_server_list, ServersList } ->
            Sender ! { self(), sever_list_updated },
            % if the new list of servers is longer than the currently stored server, update the state to the new list
            case length(ServersList) > length(Servers) of
                true -> 
                    server_actor(Users, ServersList, SubMessages);
                false -> 
                    server_actor(Users, Servers, SubMessages)
            end;

        {Sender, register_user, UserName} -> 
            ServerToStoreUser = hash_users(UserName, Servers),
            case self() =:= ServerToStoreUser of
                true -> 
                    NewUsers = dict:store(UserName, create_user(UserName), Users),
                    % io:format("Users: ~p~p~n", [[self()],NewUsers]),
                    Sender ! {self(), user_registered},
                    server_actor(NewUsers, Servers, SubMessages);
                false -> 
                    ServerToStoreUser ! {Sender, register_user, UserName},
                    server_actor(Users, Servers, SubMessages)
            end;
            
        {Sender, log_in, _UserName} -> 
            Sender ! {self(), logged_in},
            server_actor(Users, Servers, SubMessages);

        {Sender, follow, UserName, UserNameToFollow} ->
            % io:format("Servers ~p~n", [Servers]),
            ServerWithUser = hash_users(UserName, Servers),
            case self() =:= ServerWithUser of
                false -> 
                    ServerWithUser ! {Sender, follow, UserName, UserNameToFollow},
                    server_actor(Users, Servers, SubMessages);

                true -> 
                    NewUsers = follow(Users, UserName, UserNameToFollow),
                    % inverse follow relation
                    self() ! {self(), follow, UserNameToFollow, UserName},
                    Sender ! {self(), followed},
                    server_actor(NewUsers, Servers, SubMessages)
            end;

        {Sender, send_message, UserName, MessageText, Timestamp} ->
            ServerWithUser = hash_users(UserName, Servers),
            case self() =:= ServerWithUser of % TODO fix message forwarding
                true -> 
                    NewUsers = store_message(Users, {message, UserName, MessageText, Timestamp}),
                    
                    % send the message to all subscriber servers
                    {user, _Name, Subscriptions, _Messages} = get_user(UserName, Users),
                    SubscriberServers = lists:map( fun(User) -> 
                        hash_users(User, Servers)
                    end, sets:to_list(Subscriptions)),
                    lists:map( fun(Server) -> 
                        Server ! {self(), update_subscriber_messages, [{message, UserName, MessageText, Timestamp}] }
                    end, SubscriberServers),

                    Sender ! {self(), message_sent},
                    server_actor(NewUsers, Servers, SubMessages);
                false ->
                    ServerWithUser ! {Sender, send_message, UserName, MessageText, Timestamp},
                    server_actor(Users, Servers, SubMessages)
            end;

        {Sender, update_subscriber_messages, Messages } ->
            Sender ! {self(), updated_messages},
            server_actor(Users, Servers, [Messages | SubMessages]);

        {Sender, get_timeline, UserName} ->
            ServerWithUser = hash_users(UserName, Servers),
            case self() =:= ServerWithUser of
                true ->
                    {user, _Name, Subscriptions, _Messages} = get_user(UserName, Users),
                    Messages = lists:filter(fun(Message) -> 
                        [{message, MessageUserName, _MessageText, _SendTime}] = Message,
                        lists:member(MessageUserName, sets:to_list(Subscriptions))
                    end, SubMessages),

                    Sender ! {self(), timeline, UserName, sort_messages(Messages)},
                    server_actor(Users, Servers, SubMessages);
                false ->
                    ServerWithUser ! {Sender, get_timeline, UserName},
                    server_actor(Users, Servers, SubMessages)
            end;
            

        {Sender, get_profile, UserName} -> 
            ServerWithUser = hash_users(UserName, Servers),
            case self() =:= ServerWithUser of
                true ->  
                    Sender ! {self(), profile, UserName,
                    sort_messages(get_messages(Users, UserName))},
                    server_actor(Users, Servers, SubMessages);
                false -> 
                    ServerWithUser ! {Sender, get_profile, UserName },
                    server_actor(Users, Servers, SubMessages)
            end
            
    end.

% this assigns users to servers.
hash_users(UserName, ServersList) when length(ServersList) > 0 ->
    Hash = binary:decode_unsigned(crypto:hash(sha256, UserName)), % returns a number
    ServerIndex = (Hash rem length(ServersList)) + 1,
    Server = lists:nth(ServerIndex, ServersList),
    Server.

create_user(UserName) -> 
    {user, UserName, sets:new(), []}.
    
get_user(UserName, Users) ->
    % io:format("Get User: ~p~n", [Users]),
    case dict:find(UserName, Users) of
        {ok, User} -> User;
        error -> throw({user_not_found, UserName})
    end.
    
follow(Users, UserName, UserNameToFollow) -> 
    {user, Name, Subscriptions, Messages} = get_user(UserName, Users),
    NewUser = {user, Name, sets:add_element(UserNameToFollow, Subscriptions), Messages},
    dict:store(UserName, NewUser, Users).
    
store_message(Users, Message) -> 
    {message, UserName, _MessageText, _Timestamp} = Message,
    {user, Name, Subscriptions, Messages} = get_user(UserName, Users),
    NewUser = {user, Name, Subscriptions, Messages ++ [Message]},
    dict:store(UserName, NewUser, Users).
    
get_messages(Users, UserName) -> 
    {user, _, _, Messages } = get_user(UserName, Users), Messages.
    
% timeline(Users, UserName) -> % TODO Tests required for this
%     {user, _, Subscriptions, _} = get_user(UserName, Users),
%     UnsortedMessagesFromTimeline = 
%         lists:foldl(fun(FollowedUserName, AllMessages) -> 
%             AllMessages ++ get_messages(Users, FollowedUserName)
%         end,
%         [],
%         sets:to_list(Subscriptions)),
%     sort_messages(UnsortedMessagesFromTimeline).

sort_messages(Messages) ->
    lists:reverse(lists:keysort(4, lists:flatten(Messages))).
    

initialize_test(Servers) ->
    catch unregister(server_actor),
    initialize(Servers).

register_user_test() ->
    ServersList = initialize_test(4),
    % map users to servers
    [A_Server, B_Server, C_Server, D_Server | _] = lists:map( fun(User) -> hash_users(User, ServersList) end, ["A", "B", "C", "D"] ), 
    ?assertMatch({_, user_registered}, server:register_user(A_Server, "A")),
    ?assertMatch({_, user_registered}, server:register_user(B_Server, "B")),
    ?assertMatch({_, user_registered}, server:register_user(C_Server, "C")),
    ?assertMatch({_, user_registered}, server:register_user(D_Server, "D")),
    % users can map to the same server, so return the list of all servers
    [["A", "B", "C", "D"], [ServersList]].


log_in_test() ->
    [[UserName1, UserName2 | _] | [[Servers]]] = register_user_test(),
    io:format("Running Servers: ~p~n", [Servers]),
    [ServerForUser1, ServerForUser2] = lists:map( fun(User) -> hash_users(User, Servers) end, [UserName1, UserName2]),
    ?assertMatch({_Server1, logged_in}, server:log_in(ServerForUser1, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(ServerForUser2, UserName2)).

follow_test() -> 
    [[UserName1, UserName2, UserName3 | _] | [[Servers]]] = register_user_test(),
    A_S = hash_users(UserName1, Servers),
    { Server1, logged_in } = server:log_in(A_S, UserName1),
    ?assertMatch(followed, server:follow(A_S, UserName1, UserName2)),
    ?assertMatch(followed, server:follow(A_S, UserName1, UserName3)),
    {UserName1, Server1, [UserName2, UserName3]}.

send_message_test() ->
    { UserName1, Server1, Subscriptions } = follow_test(),
    ?assertMatch(message_sent, server:send_message(Server1, UserName1, "Hola, Soy Uno!")),
    ?assertMatch(message_sent, server:send_message(Server1, UserName1, "Tu bien?")),
    {UserName1, Server1, Subscriptions}.

% TODO Finish this
get_timeline_test() ->
    { UserName1, Server1, [UserName2, UserName3] } = follow_test(),
    io:format("Getting Timeline of User ~p~n", [UserName1]),

    ?assertMatch([], server:get_timeline(Server1, UserName1)),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName2, "Hello I'm B!")),

    % One message in the timeline.
    ?assertMatch([
        {message, UserName2, "Hello I'm B!", _TimeB1}
    ], server:get_timeline(Server1, UserName1)),

    ?assertMatch(message_sent,
        server:send_message(Server1, UserName2, "How is everyone?")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName3, "Hello I'm C!")),

    % All three messages in the timeline, newest first.
    ?assertMatch([
        {message, UserName3, "Hello I'm C!", _TimeC1},
        {message, UserName2, "How is everyone?", _TimeB2},
        {message, UserName2, "Hello I'm B!", _TimeB1}
    ], server:get_timeline(Server1, UserName1)),

    % User 2 does not follow any so gets an empty timeline.
    ?assertMatch([], server:get_timeline(Server1, UserName2)).

% TODO Finish this
get_profile_test() -> 
    {UserName1, Server1, [UserName2 | _]} = send_message_test(),
    % Most recent message is returned first.
    ?assertMatch([
        {message, UserName1, "Tu bien?", _TimeA2},
        {message, UserName1, "Hola, Soy Uno!", _TimeA1}
    ], server:get_profile(Server1, UserName1)),
    % User 2 hasn't sent any messages.
    ?assertMatch([], server:get_profile(Server1, UserName2)).

typical_session_test(NumberOfServers) ->
    ServersList = initialize_test(NumberOfServers),
    Session1 = spawn_link(?MODULE, typical_session_1, [self(), ServersList]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self(), ServersList]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid, ServerList) ->
    Server = hash_users("Alice", ServerList),
    {_, user_registered} = server:register_user(Server, "Alice"),
    {_AssignedServer, logged_in} = server:log_in(Server, "Alice"),
    message_sent = server:send_message(Server, "Alice", "Habari"),
    message_sent = server:send_message(Server, "Alice", "Natamai mko salama"),

    [{message, "Alice", "Natamai mko salama", Time2}, {message, "Alice", "Habari", Time1}] = server:get_profile(Server, "Alice"),
    ?assert(Time1 =< Time2),
    TesterPid ! {self(), ok}.

typical_session_2(TesterPid, ServerList) ->
    Server = hash_users("Bob", ServerList),
    {_, user_registered} = server:register_user(Server, "Bob"),
    {AssignedServer, logged_in} = server:log_in(Server, "Bob"),

    timer:sleep(1000),
    [] = server:get_timeline(AssignedServer, "Bob"),
    followed = server:follow(AssignedServer, "Bob", "Alice"),
    [{message, "Alice", "Natamai mko salama", Time2}, {message, "Alice", "Habari", Time1}] = server:get_profile(AssignedServer, "Alice"),
    ?assert(Time1 =< Time2),

    TesterPid ! {self(), ok}.

