#!/bin/bash

set -e
cd `dirname $0`
export PATH=$HOME/.local/bin:$HOME/bin:$PATH
if [ "$PROJECT_ROOT" == "" ]; then
  export PROJECT_ROOT=/react-hs
fi

# build everything
export TARGETS="\
  react-hs \
  react-hs/test/client \
  react-hs/test/spec \
  react-hs-examples \
  react-hs-examples/spec \
  react-hs-servant \
  "

for target in $TARGETS; do
  cd $PROJECT_ROOT/$target
  stack setup --allow-different-user
  stack build --allow-different-user --fast --pedantic --test
  test -e Makefile && make all
done

cd $PROJECT_ROOT/react-hs/test/spec
$PROJECT_ROOT/.travis/run-server.hs --warm-up

if [ "$1" == "--test" ]; then
    echo "starting selenium..."
    $PROJECT_ROOT/.travis/selenium.sh start
    echo "starting test app servers..."
    nohup $PROJECT_ROOT/.travis/run-server.hs 8086 $PROJECT_ROOT/react-hs-examples/ &
    nohup $PROJECT_ROOT/.travis/run-server.hs 8087 $PROJECT_ROOT/react-hs/test/client/ &
    sleep 5.3
    echo "starting tests..."
    cd $PROJECT_ROOT/react-hs-examples/spec
    stack exec -- todo-spec
    cd $PROJECT_ROOT/react-hs/test/spec
    stack exec -- react-hs-spec
    echo "done!"
fi
