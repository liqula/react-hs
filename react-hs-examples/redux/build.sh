stack setup
stack build
echo "(function(global, React, ReactDOM) {" > web/script.js
cat $(stack path --local-install-root)/bin/redux.jsexe/all.js >> web/script.js
echo "})(window, window['React'], window['ReactDOM']);" >> web/script.js
