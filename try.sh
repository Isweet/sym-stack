#!/bin/bash

cabal sandbox init
cabal install --only-dependencies --enable-tests --extra-include-dirs=/usr/local/Cellar/z3/4.4.1/include --extra-lib-dirs=/usr/local/Cellar/z3/4.4.1/lib
cabal configure --enable-tests
cabal build
./dist/build/symstack-test/symstack-test
