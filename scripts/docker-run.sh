#!/bin/bash

export DOCKER_IMAGE=fisx/react-hs-docker
export DOCKER_DIGEST=sha256:affb63c5a1ae66fbccb1edec3ba16191dce580af8f6e1446bc9f013d55623495
export DOCKER_PATH=/react-hs
export VNC_PORT=5900

export EXPOSE_VNC="-p $VNC_PORT:$VNC_PORT"
export EXPOSE_FS="-v `pwd`:$DOCKER_PATH"

# according to `--help`, `--disable-content-trust=true` is the default!
export DOCKER_ARGS=--disable-content-trust=false

case "$1" in
    "")
        docker run $DOCKER_ARGS -it --rm $EXPOSE_VNC $EXPOSE_FS $DOCKER_IMAGE@$DOCKER_DIGEST /bin/bash
        ;;
    --test)
        docker run $DOCKER_ARGS -it --rm $EXPOSE_VNC $EXPOSE_FS $DOCKER_IMAGE@$DOCKER_DIGEST ./scripts/build.sh --test
        ;;
    --populate-cache)

        echo -en "\n\nERROR: this is an untested feature.  read the script for details.\n\n"
        exit 1

        # not sure if this works.  my laptop ran out of disk
        # space after using up 11GB and freezing for 5min28secs.  this
        # indicates it won't save any time or power compared to just
        # pulling every time.  Docker version 17.06.0-ce, build 02c1d87.

        export CACHE_PATH=$HOME/.travis_cache/
        mkdir -p $CACHE_PATH/fisx/  # `/` in $DOCKER_IMAGE is interpreted as directory.
        if [ -f $CACHE_PATH/$DOCKER_IMAGE@$DOCKER_DIGEST ]; then
            docker load $CACHE_PATH/$DOCKER_IMAGE@$DOCKER_DIGEST $DOCKER_IMAGE@$DOCKER_DIGEST
        else
            docker pull $DOCKER_ARGS $DOCKER_IMAGE@$DOCKER_DIGEST
            docker save -o $CACHE_PATH/$DOCKER_IMAGE@$DOCKER_DIGEST $DOCKER_IMAGE@$DOCKER_DIGEST
        fi
        ;;
    --connect)
        docker exec -it `docker ps -q --filter="ancestor=$DOCKER_IMAGE" | head -1` /bin/bash
        ;;
    *)
        echo "bad arguments."
        exit 1
esac
