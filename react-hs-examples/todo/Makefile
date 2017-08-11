INSTALL_ROOT:=$(shell stack path --allow-different-user --local-install-root)

default: js-build/todo.js
	stack build
	npm install

js-build/todo.js: $(INSTALL_ROOT)/bin/todo.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/todo.js
	cat $(INSTALL_ROOT)/bin/todo.jsexe/all.js >> js-build/todo.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/todo.js
	sed -i 's/goog.provide.*//' js-build/todo.js
	sed -i 's/goog.require.*//' js-build/todo.js

clean:
	rm -rf js-build
