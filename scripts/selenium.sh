#!/bin/bash

set -e

export PATH=$PATH:/usr/lib/chromium-browser
export SEL=/selenium-server-standalone-3.4.0.jar
# If you want to update selenium-grid, check out https://github.com/liqula/react-hs-docker/blob/master/Dockerfile.
export SELENIUM_HOST=localhost
export SELENIUM_HUB_PORT=4444
export SELENIUM_NODE_PORT=5555
export LOG_PATH=/log
export DISPLAY=:0
export VNC_PORT=5900
export VNC_DIMS=800x600x8

export SELENIUM_ARGS="-debug true"
export SELENIUM_HUB_ARGS="-log $LOG_PATH/selenium-hub.log"
export SELENIUM_NODE_ARGS="-log $LOG_PATH/selenium-node.log"
export SELENIUM_NODE_ARGS_PRE="-Dwebdriver.gecko.driver=/usr/local/lib/geckodriver"
# (try `java -jar /selenium-server-standalone-<version>.jar [-role <node|hub>] -h` to learn more.)

# NOTE: chromedriver is started, but i the react-flux tests get a
# timeout trying to open a page, and debugging this is very awkward.
# so we stick with geckodriver (fireofox caps).
#
# tried both with ubuntu's 58.0.3029.110-0ubuntu0.16.04.1281 and with
# https://chromedriver.storage.googleapis.com/2.30/chromedriver_linux64.zip
# (sha256:342f4f8db4f9c5f14fdd8a255d2188bf735e6785d678fce93eab0b316307e474),
# adding -Dwebdriver.chrome.driver=/usr/local/lib/chromedriver to
# $SELENIUM_NODE_ARGS_PRE above.


case "$1" in
  start)
    cd /tmp/  # this is all dumping a lot of cores...
    mkdir -p $LOG_PATH

    echo -n "starting Xvfb"
    nohup Xvfb $DISPLAY -screen 0 $VNC_DIMS > $LOG_PATH/selenium-xvfb.log 2>&1 &
    until (xdpyinfo -display $DISPLAY >/dev/null 2>&1); do echo -n "."; sleep .$RANDOM; done
    echo " ok"

    # if you want to run the react-hs-docker image locally and connect
    # to selenium while it is running, activate this block by
    # replacing the leading `false` with `true`.
    false && \
        echo -n "starting vnc" && \
        (nohup x11vnc -forever -rfbport $VNC_PORT -display $DISPLAY > $LOG_PATH/selenium-x11vnc.log 2>&1 & ) && \
        ( until (nc -z $SELENIUM_HOST $VNC_PORT); do echo -n "."; sleep .$RANDOM; done ) && \
        echo " ok"

    echo -n "starting hub"
    nohup java -jar $SEL -role hub $SELENIUM_ARGS $SELENIUM_HUB_ARGS \
          -host $SELENIUM_HOST -port $SELENIUM_HUB_PORT &
    until (nc -z $SELENIUM_HOST $SELENIUM_HUB_PORT); do echo -n "."; sleep .$RANDOM; done
    echo " ok"

    echo -n "starting node"
    nohup java $SELENIUM_NODE_ARGS_PRE -jar $SEL -role node $SELENIUM_ARGS $SELENIUM_NODE_ARGS \
          -host $SELENIUM_HOST -port $SELENIUM_NODE_PORT \
          -hub http://$SELENIUM_HOST:$SELENIUM_HUB_PORT/grid/register &
    until (nc -z $SELENIUM_HOST $SELENIUM_NODE_PORT); do echo -n "."; sleep .$RANDOM; done
    echo " ok"
    ;;

  stop)
    killall -q -9 java
    killall -q -9 Xvfb
    killall -q -9 x11vnc
    ;;

  restart)
    $0 stop
    $0 start
    ;;

  *)
    echo "bad arguments"
    ;;

esac
