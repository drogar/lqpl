#!/bin/bash
# uses environment varible LQPL_SERVICE to determine which server to run
# If not set, will run the emulator

if [[ -d "/opt/lqpl" ]]
then
    run_directory="/opt/lqpl/bin/"
else
    run_directory="$HOME/programming/mixed/lqpl/"
fi

service="lqpl-emulator"

if [[ "$LQPL_SERVICE" == "COMPILER" ]]
then
   service="lqpl-compiler-server"
fi

echo "Running $run_directory$service"

$run_directory$service
