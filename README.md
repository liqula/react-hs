A GHCJS binding to [React](https://facebook.github.io/react/) based on the
[Flux](https://facebook.github.io/flux/) design.  The flux design pushes state and complicated logic
out of the view, allowing the rendering functions and event handlers to be pure Haskell functions.
When combined with React's composable components and the one-way flow of data, React, Flux, and
GHCJS work very well together.


# Project Status

*EXPERIMENTAL*


# Getting started

Add this to your stack.yaml:

```yaml
packages:
- location:
    git: https://github.com/liqula/react-hs
    commit: 8eb9d31fcfa0a79ef006565f299d47b210bdd690  # please check if that's still the most recent commit on master.
  extra-dep: true
  subdirs:
  - react-hs
  - react-hs-servant
  - react-hs-examples
```

Then read the rest of this README and the READMEs in the packages.


# Details

This is a well-maintained fork of three unmaintained bitbucket
repositories:

- https://bitbucket.org/wuzzeb/react-flux/commits/2be756ffa5d60bdc15f767f4006f0e145efcb39e
- https://bitbucket.org/wuzzeb/react-flux-servant/commits/86f086f71667197aae15ee48672207ba0012e98c
- https://bitbucket.org/wuzzeb/react-flux-example/commits/ecdb19e3264b7b23fed92a28eb8a89b1191de935

We are currently working on getting the change logs and issue lists
updated.  Having this said and with the caveat that you may have to
read a lot of source code to get it to do what you want, we believe
this version already works a lot better than the original.

Package version numbers are chosen in accordance with
https://pvp.haskell.org/.

See the package READMEs for more info.

Please vote on the open issues to help decide which to work on first.


# Changes since react-flux

- use haskell equality on stores, props, states in `shouldComponentUpdate`.
- work-around https://github.com/ghcjs/ghcjs/issues/570: compute store keys in `registerInitialStore` by hand.
- work-around https://github.com/ghcjs/ghcjs/issues/556: do not use `ToJSVal`, `FromJSVal` instances for aeson `Value` type.
- simplify `preventDefault`, `stopPropagation` (eliminates a bizarre and beautiful, but unnecessary hack).
- set up travis ci.
- lots of small api improvements.
- remove lots of deprecated code.
- remove CPP clutter for building with ghc (not ghcjs).


## Migration

If you have code that works with react-flux 1.2.3 (the latest on
hackage as of the time of this fork), you need to make a few changes.
Refer to the TODO app in react-hs-examples for some of the details.

- instead of `renderSomeView`, call `mkSomeView` and follow the type errors; except lifecycle views which still use the old types (see #7).
- use `View` instead of `ReactView`.
- use `view` instead of `view_`.
- `mkStore` is gone, you need to call `createInitialStore` in the beginning of your main function.
- calls to `mkControllerView` contain a type argument that is a list of all stores that you want to pass to the function that constructs the content.
- make Eq instances for all your prop, state, store types ([check out the example](https://github.com/liqula/react-hs/blob/a5d2d88f6da91a2243ee5cc9ca608e1580a4139d/react-hs-examples/src/TodoComponents.hs#L28) if you have un-Eq-able types like functions.)
- add lines `instance UnoverlapAllEq X` for all types `X` that give you type errors.  [you can't do anything wrong here as long as you do not instantiate 'StoreArg' or 'StoreField'.  this is just a hack to sort out overlapping instances.](https://github.com/liqula/react-hs/blob/41f325790b9a8f4ca75d88a5d8c18dc145f3c1e3/react-hs/src/React/Flux/Internal.hs#L593)
- `stopPropagation`, `preventDefault` are used differently now.  you should run them first thing in the event handler, like this: `div_ [onClick $ \evt _ -> stopPropagation evt `seq` dispatch MyAction] $ ...`.

I think that's the gist of it, but I'm pretty sure i forgot a few
interesting details.  If you need more details or run into any
questions, please open an issue.


# Further reading

- http://blog.wuzzeb.org/
- http://iankduncan.com/posts/2014-12-16-react-js-with-ghcjs.html
