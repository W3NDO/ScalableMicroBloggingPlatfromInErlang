-module(distributed_benchmark).

-export([test_timeline/1, test_send_message/1, test_get_profile/1, test_system_load/2]).

run_benchmark(Name, Fun, Times, NumberOfServers) ->
    io:format("run,wall_clock_time,cpu_time,number_of_servers,benchmark~n"),
    ThisPid = self(),
    lists:foreach(fun(N) -> 
        spawn_link(fun() -> 
            run_benchmark_once(Name, Fun, N, NumberOfServers),
            ThisPid ! done
        end),
        receive done -> 
            ok
        end
    end, lists:seq(1, Times)).

run_load_benchmark(Name, Fun, Times, NumberOfServers) ->
    io:format("run,number_of_servers,benchmark_name,average_message_queue_length~n"),
    ThisPid = self(),
    lists:foreach(
        fun(N) ->
           spawn_link(fun() ->
                run_load_benchmark_once(Name, Fun, N, NumberOfServers), ThisPid ! done
            end) ,
            receive done ->
                ok
            end
        end, lists:seq(1,Times)).

run_load_benchmark_once(Name, Fun, N, NumberOfServers) ->
    Fun(),
    io:format("~p,~p,~s,", [N,NumberOfServers,Name]).

run_benchmark_once(Name, Fun, N, NumberOfServers) ->
    StartTime = os:timestamp(),
    statistics(runtime),
    Fun(),
    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    {_, CpuTime} = statistics(runtime),
    io:format("~p,~p,~p,~p,~s~n", [N,WallClockTime / 10000.0, CpuTime,NumberOfServers,Name]).

initialize_server(PassedNumberOfServers) ->
    NumberOfServers = PassedNumberOfServers,
    rand:seed_s(exsplus, {0, 0, 0}),
    NumberOfUsers = 2000,
    NumberOfSubscriptions = 10,
    NumberOfMessages = 30,

    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    Users = dict:from_list(lists:map(fun(Name) -> 
        Subscriptions = [pick_random(UserNames) || _ <- lists:seq(1,NumberOfSubscriptions)],
        Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],
        User = {user, Name, sets:from_list(Subscriptions), Messages},
        {Name, User}
    end, UserNames )),
    ServerPids = server_distributed:initialize_with(Users, NumberOfServers),
    {ServerPids, UserNames}.

pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).

generate_message(UserName, I) ->
    Text = "Message" ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

test_timeline(NumberOfServers) -> % TODO Finish this
    {ServerPids, UserNames} = initialize_server(NumberOfServers),
    run_benchmark("Timeline", 
        fun() ->
            lists:foreach(
                fun (_) -> 
                    server:get_timeline(pick_random(ServerPids), pick_random(UserNames))
                end, lists:seq(1, 10000)
            )
        end, 30, NumberOfServers).

test_send_message(NumberOfServers) ->
    {ServerPids, UserNames} = initialize_server(NumberOfServers),
    run_benchmark("send_message",
        fun () ->
            lists:foreach(fun (_) ->
                server:send_message(pick_random(ServerPids), pick_random(UserNames), "Test")
            end,
            lists:seq(1, 10000))
        end,
        30, NumberOfServers).

test_get_profile(NumberOfServers) ->
    {ServerPids, UserNames} = initialize_server(NumberOfServers),
    run_benchmark("get_profile",
        fun () ->
            lists:foreach(fun (_) ->
                server:send_message(pick_random(ServerPids), pick_random(UserNames), "Test")
            end,
            lists:seq(1, 1000)),
            lists:foreach(fun (_) ->
                server:get_profile(pick_random(ServerPids), pick_random(UserNames))
            end,
            lists:seq(1, 10000))
        end,
        30, NumberOfServers).

test_system_load(NumberOfServers, NumberOfMessagesSent) ->
    % io:format("Testing The average system Load~n"),
    {ServerPids, UserNames} = initialize_server(NumberOfServers),
    run_load_benchmark("system_load", 
        fun() ->
            lists:foreach(fun (_) ->
                server:send_message(pick_random(ServerPids), pick_random(UserNames), "Test")
            end,
            lists:seq(1, NumberOfMessagesSent)),

            Average = sum(lists:map( 
                fun(Server) ->
                    {message_queue_len, NumberOfMessagesInServer} = erlang:process_info(Server, message_queue_len),
                    NumberOfMessagesInServer
                end, ServerPids)),
                io:format("~p~n", [Average])
        end, 30, NumberOfServers).

sum(L) -> 
    sum(L, 0).
    
sum([H|T], Acc) -> 
    sum(T, H + Acc); 
    
sum([], Acc) ->
    Acc.