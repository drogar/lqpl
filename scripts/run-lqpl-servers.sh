#!/bin/bash
# uses environment varible LQPL_SERVICE to determine which server to run
# If not set, will run the emulator

if [[ -d "/opt/lqpl" ]]
then
    run_directory="/opt/lqpl/bin/"
else
    run_directory="$HOME/programming/mixed/lqpl/"
fi

service="lqpl_emulator"

if [[ "$LQPL_SERVICE" == "COMPILER" ]]
then
   service="lqpl_compiler_emulator"
fi

echo "Running $run_directory$service"
