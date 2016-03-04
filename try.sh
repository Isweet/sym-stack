#!/bin/bash

cabal sandbox init
cabal install --only-dependencies --enable-tests --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
cabal configure --enable-tests
cabal build
cabal test --show-details=streaming

