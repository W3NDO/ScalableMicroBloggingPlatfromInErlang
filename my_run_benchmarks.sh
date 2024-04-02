#!/usr/bin/env bash

set -euo pipefail

for i in {1..64}
do
    echo "---"
    echo "> send message, $i threads"
    erl +S $i -noshell -s my_benchmark test_send_message -s init stop > "benchmarks/firefly/result-send-message-$i.txt"

    echo "---"
    echo "> timeline, $i threads"
    erl +S $i -noshell -s my_benchmark test_timeline -s init stop > "benchmarks/firefly/result-timeline-$i.txt"
done
