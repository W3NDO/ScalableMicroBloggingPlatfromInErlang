-module(my_benchmark).

-export([test_send_message/0, test_timeline/0]).

run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(fun (N) -> 
        spawn_link( fun () -> 
            run_benchmark_once(Name, Fun, N), 
            ThisPid ! done
        end),
        receive done -> 
            ok
        end
    end, lists:seq(1, Times)).

run_benchmark_once(Name, Fun, N) ->
    io:format("Starting benchmark ~s: ~p~n", [Name, N]),
    StartTime = os:timestamp(),
    statistics(runtime),

    Fun(),

    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    {_, CpuTime} = statistics(runtime),
    io:format("Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
    io:format("CPU time = ~p ms~n", [CpuTime]),
    io:format("~s done~n", [Name]).

initialize_server() ->
    rand:seed_s(exsplus, {0, 0, 0}),
    NumberOfUsers = 1000,
    NumberOfSubscriptions = 100,
    NumberOfMessages = 25,
    io:format("Parameters:~n"),
    io:format("Number of users : ~p~n", [NumberOfUsers]),
    io:format("Number of Subscriptions : ~p~n", [NumberOfSubscriptions]),
    io:format("Number of messages : ~p~n", [NumberOfMessages]),
    io:format("~n"),


    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],

    Users = dict:from_list(lists:map(fun (Name) ->
        Subscriptions = [pick_random(UserNames) || _ <- lists:seq(1, NumberOfSubscriptions)],

        Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],

        User = {user, Name, sets:from_list(Subscriptions), Messages},

        {Name, User}
        end, UserNames)),
    
    ServerPid = server_individual:initialize_with(Users), { ServerPid, UserNames}.

pick_random(List) -> 
    lists:nth(rand:uniform(length(List)), List).

generate_message(UserName, I) -> 
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

test_timeline() -> 
    {ServerPid, UserName} = initialize_server(),
    run_benchmark("timeline", 
        fun () -> 
            lists:foreach(
                fun(_) ->
                    server:get_timeline(ServerPid, pick_random(UserName))
                end, 
                lists:seq(1,10000))
        end, 30).

test_send_message() -> 
    {ServerPid, UserName} = initialize_server(),
    run_benchmark("send message", 
        fun() -> 
           lists:foreach( 
                fun(_) ->
                    server:send_message(ServerPid, pick_random(UserName), "Test")
                end,
                lists:seq(1,10000)) 
        end, 30).
