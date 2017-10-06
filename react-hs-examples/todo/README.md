This directory contains three example applications.

# Todo Example

This is the simpler of the two examples, and you should start here.  The design
is copied pretty much exactly from the [flux todo
example](https://github.com/facebook/flux/tree/master/examples/flux-todomvc).
It uses the same actions, same views, and produces the same DOM, so the design
overview from the flux repository covers this example application as well.

When reading the code for the example application, you should start with `TodoStore.hs`.  Next, look
at `TodoDispatcher.hs` and `TodoViews.hs`.  Finally, you can look at `TodoComponents.hs` and
`Main.hs` and `NodeMain.hs`.

### Build

To build, run `stack build` followed by `make` in the top-level directory.  The makefile sets up a symbolic
link to the stack output directory and also compresses the resulting javascript using closure.

### TODO in the browser

A result of the build is in the directory `js-build/install-root/bin/todo.jsexe`.  There is a file
`example/todo/todo-dev.html` which loads this `all.js` file from this directory, so you
can open `todo-dev.html` after building.

But to deploy a react-hs application, you should minimize it since the size
of `all.js` is 1.8 mebibytes.  To do so, the `Makefile` calls closure to
produce a file `js-build/todo.min.js`.  Then the `todo.html` references this
minimized javascript file, which is only 500 kibibytes which when compressed
with gzip is 124 kibibytes.

### TODO in node

`NodeMain.hs` is a separate main module which instead of rendering the TODO example application into the DOM,
it renders it to a string and then displays it.  To execute this, run

~~~
cd example/todo
npm install react@15.3.0 react-dom@15.3.0
node run-in-node.js
~~~

### Testing

Finally, you might be interested to look at
[test/spec/TodoSpec.hs](https://bitbucket.org/wuzzeb/react-flux/src/tip/test/spec/TodoSpec.hs) as it
contains an [hspec-webdriver](https://hackage.haskell.org/package/hspec-webdriver) spec for the TODO
example application.
