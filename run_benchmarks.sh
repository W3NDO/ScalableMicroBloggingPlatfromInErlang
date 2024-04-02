#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

MAX_SERVERS=640 # TODO change to 640 on firefly
echo "---"
echo "Experiment 3: System Load against Number of Servers using Max number of Threads"
MIN_SERVERS=10
NUMBER_OF_MESSAGES_SENT=200
for ((i=MAX_SERVERS; i>=MIN_SERVERS; i-=80))
do
    echo "$i"
    echo "---"
    echo "> test system load, $i Server(s)"
    echo "> Sending $NUMBER_OF_MESSAGES_SENT messages"
    echo "> Number of servers $i "
    erl -noshell -s -eval "distributed_benchmark:test_system_load($i,$NUMBER_OF_MESSAGES_SENT)" -s init stop > "firefly_benchmarks/experiment3/exp3-get-profile-$i.csv"
    echo "---"
done

# MAX_THREADS=256
# echo "---Experiment 1: Fixed Number of Servers, varied number of threads ---"
# #firefly
# for ((i=256; i>=1; i-=21)) # 6 loops
# do
#     SERVERS=600
#     echo "---"
#     echo "> timeline, $i thread(s)"
#     echo "> Number of servers $SERVERS "
#     erl +S $i -noshell -s -eval "distributed_benchmark:test_timeline($SERVERS)" -s init stop > "firefly_benchmarks/experiment1_run2/exp1-timeline-$i.csv"
    
#     echo "---"
#     echo "> get_profile, $i thread(s)"
#     echo "> Number of servers $SERVERS "
#     erl +S $i -noshell -s -eval "distributed_benchmark:test_get_profile($SERVERS)" -s init stop > "firefly_benchmarks/experiment1_run2/exp1-get-profile-$i.csv"
    
#     echo "---"
#     echo "> send_message, $i thread(s)"
#     echo "> Number of servers $SERVERS "
#     erl +S $i -noshell -s -eval "distributed_benchmark:test_send_message($SERVERS)" -s init stop > "firefly_benchmarks/experiment1_run2/exp1-send-message-$i.csv"
# done


# #local
# # for ((i=16; i>=1; i-=2)) # TODO  change to 128 & 4 on firefly
# # do
# #     SERVERS=50 # TODO change to 400 on firefly
# #     echo "---"
# #     echo "> timeline, $i thread(s)"
# #     echo "> Number of servers $SERVERS "
# #     erl +S $i -noshell -s -eval "distributed_benchmark:test_timeline($SERVERS)" -s init stop > "benchmarks/experiment1/exp1-timeline-$i.csv"
    
# #     echo "---"
# #     echo "> get_profile, $i thread(s)"
# #     echo "> Number of servers $SERVERS "
# #     erl +S $i -noshell -s -eval "distributed_benchmark:test_get_profile($SERVERS)" -s init stop > "benchmarks/experiment1/exp1-get-profile-$i.csv"
    
# #     echo "---"
# #     echo "> send_message, $i thread(s)"
# #     echo "> Number of servers $SERVERS "
# #     erl +S $i -noshell -s -eval "distributed_benchmark:test_send_message($SERVERS)" -s init stop > "benchmarks/experiment1/exp1-send-message-$i.csv"
# # done

# echo "---"
# echo "Experiment 2: Fixed Number of Threads(Max), varied number of Servers"
# MIN_SERVERS=10
# for ((i=MAX_SERVERS; i>=MIN_SERVERS; i-=10))
# do
#     echo "$i"
#     echo "---"
#     echo "> timeline, $i Server(s)"
#     echo "> Number of servers $i "
#     erl +S $MAX_THREADS -noshell -s -eval "distributed_benchmark:test_timeline($i)" -s init stop > "firefly_benchmarks/experiment2_run2/exp2-timeline-$i.csv"
    
#     echo "---"
#     echo "> get_profile, $i Server(s)"
#     echo "> Number of servers $i "
#     erl +S $MAX_THREADS -noshell -s -eval "distributed_benchmark:test_get_profile($i)" -s init stop > "firefly_benchmarks/experiment2_run2/exp2-get-profile-$i.csv"
#     echo "---"

#     echo "---"
#     echo "> send_message, $i Server(s)"
#     echo "> Number of servers $i "
#     erl +S $MAX_THREADS -noshell -s -eval "distributed_benchmark:test_send_message($i)" -s init stop > "firefly_benchmarks/experiment2_run2/exp2-send-message-$i.csv"

# done

