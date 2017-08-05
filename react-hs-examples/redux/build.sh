stack setup --stack-yaml stackjs.yaml
stack build --stack-yaml stackjs.yaml
echo "(function(global, React, ReactDOM) {" > web/script.js
cat $(stack path --stack-yaml stackjs.yaml --local-install-root)/bin/redux.jsexe/all.js \
    >> web/script.js
echo "})(window, window['React'], window['ReactDOM']);" >> web/script.js
