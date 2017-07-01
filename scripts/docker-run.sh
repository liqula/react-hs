#!/bin/bash

export DOCKER_IMAGE=fisx/react-hs-docker
export DOCKER_PATH=/react-hs
export VNC_PORT=5900

if [ "$1" == "--connect" ]; then
    docker exec -it `docker ps -q --filter="ancestor=$DOCKER_IMAGE" | head -1` /bin/bash
else
    docker pull $DOCKER_IMAGE
    docker run --rm -p $VNC_PORT:$VNC_PORT -it -v `pwd`:$DOCKER_PATH $DOCKER_IMAGE /bin/bash
fi
