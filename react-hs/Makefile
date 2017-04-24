INSTALL_ROOT:=$(shell stack path --local-install-root)

#CLOSURE_COMPILER=closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS
CLOSURE_COMPILER=cat

all: js-build/install-root js-build/test-client.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/test-client.min.js: js-build/test-client.js
	$(CLOSURE_COMPILER) js-build/test-client.js > js-build/test-client.min.js

js-build/test-client.js: $(INSTALL_ROOT)/bin/test-client.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/test-client.js
	cat $(INSTALL_ROOT)/bin/test-client.jsexe/all.js >> js-build/test-client.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/test-client.js
	sed -i 's/goog.provide.*//' js-build/test-client.js
	sed -i 's/goog.require.*//' js-build/test-client.js

clean:
	rm -rf js-build
