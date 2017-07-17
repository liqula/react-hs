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
  echo -en "\n\n>>> $PROJECT_ROOT/$target\n\n"
  cd $PROJECT_ROOT/$target
  test -e Makefile && make npm
  stack setup --allow-different-user
  stack clean --allow-different-user
  stack build --allow-different-user $REACT_HS_BUILD_WHAT --fast --pedantic --test
  test -e Makefile && make default
done

echo -en "\n\n>>> building webserver for testing...\n\n"

cd $PROJECT_ROOT/react-hs/test/spec
$PROJECT_ROOT/scripts/run-server.hs --warm-up

echo -en "\n\n>>> done!\n\n"

if [ "$1" == "--test" ]; then
    echo "starting selenium..."
    $PROJECT_ROOT/scripts/selenium.sh start
    echo "starting test app servers..."
    nohup $PROJECT_ROOT/scripts/run-server.hs 8086 $PROJECT_ROOT/react-hs-examples/ &
    nohup $PROJECT_ROOT/scripts/run-server.hs 8087 $PROJECT_ROOT/react-hs/test/client/ &
    sleep 5.3
    curl -v curl http://localhost:8086/html/todo.html 2>&1 | grep -q script
    curl -v curl http://localhost:8087/test-client.html 2>&1 | grep -q script
    echo "starting tests..."
    cd $PROJECT_ROOT/react-hs-examples/spec
    stack exec --allow-different-user -- todo-spec
    cd $PROJECT_ROOT/react-hs/test/spec
    stack exec --allow-different-user -- react-hs-spec
    echo "done!"
fi
