#!/bin/bash

export DOCKER_IMAGE=fisx/react-hs-docker
export DOCKER_DIGEST=sha256:affb63c5a1ae66fbccb1edec3ba16191dce580af8f6e1446bc9f013d55623495
export DOCKER_PATH=/react-hs
export VNC_PORT=5900

if [ "$1" == "--connect" ]; then
    docker exec -it `docker ps -q --filter="ancestor=$DOCKER_IMAGE" | head -1` /bin/bash
else
    docker pull $DOCKER_IMAGE@$DOCKER_DIGEST
    docker run --rm -p $VNC_PORT:$VNC_PORT -it -v `pwd`:$DOCKER_PATH $DOCKER_IMAGE@$DOCKER_DIGEST /bin/bash
fi
