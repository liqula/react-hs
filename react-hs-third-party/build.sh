#!/bin/bash

cabal build
cp dist/build/react-hs-third-party/react-hs-third-party.jsexe/all.js html/all.js
