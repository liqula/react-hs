#!/bin/bash

set -e

INSTALL_ROOT=$(stack path --allow-different-user --local-install-root)

rm -rf js-build
mkdir js-build

npm install
stack build --allow-different-user

echo "(function(global,React,ReactDOM) {" > js-build/todo.js
cat ${INSTALL_ROOT}/bin/todo.jsexe/all.js >> js-build/todo.js
echo "})(window, window['React'], window['ReactDOM']);" >> js-build/todo.js
sed -i 's/goog.provide.*//' js-build/todo.js
sed -i 's/goog.require.*//' js-build/todo.js
