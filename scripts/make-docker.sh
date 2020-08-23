#!/bin/bash
if [ -x 'Dockerfile' ]
then
    echo 'Must be run in directory with the Docker file'
    exit 1
fi
docker build -t lqpl:build .
docker tag lqpl:build brettgiles/lqpl-servers:latest
docker push brettgiles/lqpl-servers:latest
