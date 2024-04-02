-module(server_individual).

-include_lib("eunit/include/eunit.hrl").

-export([initialize/0, initialize_with/1, server_actor/1]).

initialize() -> 
    initialize_with(dict:new()).

initialize_with(Users) ->
    % io:format("~p~n", [Users]),
    ServerPid = spawn_link(?MODULE, server_actor, [Users]),
    catch unregister(server_actor),
    register(server_actor, ServerPid),
    ServerPid.
% 
server_actor(Users) ->
    receive
        {Sender, register_user, UserName} -> 
            NewUsers = dict:store(UserName, create_user(UserName), Users),
            Sender ! {self(), user_registered},
            server_actor(NewUsers);

        {Sender, log_in, _UserName} -> 
            Sender ! {self(), logged_in},
            server_actor(Users);

        {Sender, follow, UserName, UserNameToFollow} ->
            NewUsers = follow(Users, UserName, UserNameToFollow),
            Sender ! {self(), followed},
            server_actor(NewUsers);

        {Sender, send_message, UserName, MessageText, Timestamp} ->
            NewUsers = store_message(Users, {message, UserName, MessageText, Timestamp}),
            Sender ! {self(), message_sent},
            server_actor(NewUsers);

        {Sender, get_timeline, UserName} ->
            Sender ! {self(), timeline, UserName, timeline(Users, UserName)},
            server_actor(Users);

        {Sender, get_profile, UserName} -> 
            Sender ! {self(), profile, UserName,
            sort_messages(get_messages(Users, UserName))},
            server_actor(Users)
    end.

create_user(UserName) -> 
    {user, UserName, sets:new(), []}.

get_user(UserName, Users) -> 
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

timeline(Users, UserName) -> 
    {user, _, Subscriptions, _} = get_user(UserName, Users),
    UnsortedMessagesFromTimeline = 
        lists:foldl(fun(FollowedUserName, AllMessages) -> 
            AllMessages ++ get_messages(Users, FollowedUserName)
        end,
        [],
        sets:to_list(Subscriptions)),
    sort_messages(UnsortedMessagesFromTimeline).

sort_messages(Messages) ->
    lists:reverse(lists:keysort(4, Messages)).

% Tests
initialize_test() -> 
    catch unregister(server_actor),
    initialize().

register_user_test() -> 
    initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "A")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "B")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "C")),
    ?assertMatch({_, user_registered}, server:register_user(server_actor, "D")),
    ["A", "B", "C", "D"].

follow_test() -> 
    [UserName1, UserName2, UserName3| _] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_actor, UserName1),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName2)),
    ?assertMatch(followed, server:follow(Server1, UserName1, UserName3)),
    {UserName1, Server1, [UserName2, UserName3]}.

send_message_test() -> 
    {UserName1, Server1, Subscriptions} = follow_test(),
    ?assertMatch(message_sent, server:send_message(Server1, UserName1, "Hello There!")),
    ?assertMatch(message_sent, server:send_message(Server1, UserName1, "General Kenobi!")),
    {UserName1, Server1, Subscriptions}.

get_timeline_test() -> 
    {UserName1, Server1, [UserName2, UserName3]} = follow_test(),

    ?assertMatch([], server:get_timeline(Server1, UserName1)),

    ?assertMatch(message_sent, server:send_message(Server1, UserName2, "It's over Anakain, I have the high ground")),

    ?assertMatch([{message, UserName2, "It's over Anakain, I have the high ground", _TimeB1}], server:get_timeline(Server1, UserName1)),

    ?assertMatch(message_sent,
        server:send_message(Server1, UserName2, "I hate sand. It's coarse and it gets everywhere")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName3, "Do It!")),

    ?assertMatch([
        {message, UserName3, "Do It!", _TimeC1},
        {message, UserName2, "I hate sand. It's coarse and it gets everywhere", _TimeB2},
        {message, UserName2, "It's over Anakin, I have the high ground", _TimeB1}
    ], server:get_timeline(Server1, UserName1)),

    ?assertMatch([], server:get_timeline(Server1, UserName2)).