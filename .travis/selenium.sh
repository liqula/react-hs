#!/bin/bash

set -e

export PATH=$PATH:/usr/lib/chromium-browser
export SEL=/selenium-server-standalone-3.4.0.jar
# If you want to update selenium-grid, check out https://github.com/liqula/react-hs-docker/blob/master/Dockerfile.
export SELENIUM_HOST=localhost
export SELENIUM_HUB_PORT=4444
export SELENIUM_NODE_PORT=5555
export LOG_PATH=/log
export DISPLAY=:1
export VNC_PORT=5900
export VNC_DIMS=800x600x8

export SELENIUM_HUB_ARGS="-debug true"
export SELENIUM_NODE_ARGS="-debug true"
# (try `java -jar /selenium-server-standalone-<version>.jar [-role <node|hub>] -h` to learn more.)

# chromedriver is started, but i the react-flux tests get a
# timeout trying to open a page.  but who cannot connect to whom?
# try standalone senelium grid with -log, -debug next!
# also, is chrome driver connecting to the right display?  shouldn't we see it on vnc?


case "$1" in
  start)
    cd /tmp/  # this is all dumping a lot of cores...
    mkdir -p $LOG_PATH

    echo -n "starting Xvfb"
    nohup Xvfb $DISPLAY -screen 0 $VNC_DIMS > $LOG_PATH/selenium-xvfb.log 2>&1 &
    until (xdpyinfo -display $DISPLAY >/dev/null 2>&1); do echo -n "."; sleep .$RANDOM; done
    echo " ok"

# if you want to run the react-hs-docker image locally and connect to selenium while it is running, uncomment this block.
    echo -n "starting vnc"
    nohup x11vnc -forever -rfbport $VNC_PORT -display $DISPLAY > $LOG_PATH/selenium-x11vnc.log 2>&1 &
    until (nc -z $SELENIUM_HOST $VNC_PORT); do echo -n "."; sleep .$RANDOM; done
    echo " ok"

    echo -n "starting hub"
    nohup java -jar $SEL -role hub -host $SELENIUM_HOST -port $SELENIUM_HUB_PORT \
          $SELENIUM_HUB_ARGS \
          > $LOG_PATH/selenium-hub.log 2>&1 &
    until (nc -z $SELENIUM_HOST $SELENIUM_HUB_PORT); do echo -n "."; sleep .$RANDOM; done
    echo " ok"

    echo -n "starting node"
    nohup java -Dwebdriver.gecko.driver=/usr/local/lib/geckodriver -jar $SEL -role node -host $SELENIUM_HOST -port $SELENIUM_NODE_PORT \
          -hub http://$SELENIUM_HOST:$SELENIUM_HUB_PORT/grid/register \
          $SELENIUM_NODE_ARGS \
          > $LOG_PATH/selenium-node.log 2>&1 &
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
