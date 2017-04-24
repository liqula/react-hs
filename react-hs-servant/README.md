This project integrates [react-flux](http://hackage.haskell.org/package/react-flux) with
[servant](http://hackage.haskell.org/package/servant).  There is a small example in the
haddock documentation.  To depend on this module, I add it to the `extra-deps` in my
project's `stack.yaml`.  See the [react-flux](http://hackage.haskell.org/package/react-flux)
documentation for a full example `stack.yaml`.

At the moment the code works well and is straightforward to integrate into a react-flux
store; I am using it in production.  Having said that, the current API requires you
to create two store actions for each request and it is somewhat tedious to write all
this boilerplate.  At some point, I would like to build a better API, perhaps one that
takes better advantage of the type-level Servant.API definitions to drastically reduce
the boilerplate.  Unfortunately, since it currently works (and even works well, the existing
API is good), due to time constraints it might be awhile before I get around to thinking about
better APIs.  If you have any ideas for a better API, even if you don't have any code,
please open an issue!