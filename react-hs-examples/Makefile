INSTALL_ROOT:=$(shell stack path --local-install-root)

#CLOSURE_COMPILER=closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS
CLOSURE_COMPILER=cat

all: js-build/install-root js-build/todo.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/todo.min.js: js-build/todo.js
	$(CLOSURE_COMPILER) js-build/todo.js > js-build/todo.min.js

js-build/todo.js: $(INSTALL_ROOT)/bin/todo.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/todo.js
	cat $(INSTALL_ROOT)/bin/todo.jsexe/all.js >> js-build/todo.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/todo.js
	sed -i 's/goog.provide.*//' js-build/todo.js
	sed -i 's/goog.require.*//' js-build/todo.js

clean:
	rm -rf js-build
