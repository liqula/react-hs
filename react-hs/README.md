A GHCJS binding to [React](https://facebook.github.io/react/) based on the
[Flux](https://facebook.github.io/flux/) design.  The flux design pushes state and complicated logic
out of the view, allowing the rendering functions and event handlers to be pure Haskell functions.
When combined with React's composable components and the one-way flow of data, React, Flux, and
GHCJS work very well together.


# Docs

The [haddocks](https://hackage.haskell.org/package/react-flux) contain the documentation.

# Using in your own project

I use stack to build my frontend which uses react-flux.  I set up stack and
ghcjs using [these
instructions](http://docs.haskellstack.org/en/stable/ghcjs/).  Note that
react-flux requires GHCJS master (a.k.a. improved base).  I keep updated with
the latest version of GHCJS and lts.  As I write this, I use the following `stack.yaml`,
but as new LTS versions or new GHCJS versions come out I keep updating to them (and
probably forget to update this README :)

~~~
resolver: lts-7.19
compiler: ghcjs-0.2.1.9007019_ghc-8.0.1
compiler-check: match-exact
packages:
- .
- location:
    git: https://github.com/liqula/react-flux
    commit: fc09983c313536d51278f9fa3bdbc10dd9d1149e
  extra-dep: true
~~~

# Test Suite

To run the test suite, first you must build both the example applications and
the test-client using ghcjs.  (The test-client is a react-flux application
which contains code for everything not contained in the todo example.)  This is
the first `stack build` below.  Then, you must build the test suite, which is a
haskell application using
[hspec-webdriver](https://hackage.haskell.org/package/hspec-webdriver).  This
must be built using GHC (not GHCJS), so there is a separate `stack.yaml` file
in the `test/spec` directory.  Also, `react-intl` must be installed in the test client
directory.  In summary, run the following commands:

~~~ {.bash}
cd test/client
npm install react-intl
cd test/spec
stack build
~~~

Next, install [selenium-server-standalone](http://www.seleniumhq.org/download/) (also from
[npm](https://www.npmjs.com/package/selenium-server-standalone-jar)).

Finally, start selenium-server-standalone and execute the test suite.  Make sure you also have
closure installed, since the test suite will compress the todo app before testing it.  It must be
started from the `test/spec` directory, otherwise it does not find the correct javascript files.

~~~
cd test/spec
stack exec react-flux-spec
~~~

# Other Projects

It differes significantly from the other two react bindings,
[react-haskell](https://github.com/joelburget/react-haskell) and
[ghcjs-react](https://github.com/fpco/ghcjs-react).  In particular, the major difference is how
events are handled.  In the Flux design, the state is moved out out of the view and then handlers
produce actions which transform the state.  Thus there is a one-way flow of data from the store into
the view.  In contrast, react-haskell and ghcjs-react both have event signals propagaing up the
react component tree, transforming state at each node.  In particular, react-haskell with its InSig
and OutSig have the signals propagate up the tree and optionally transform state at each node and
change the type of the signal.
