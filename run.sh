#! /bin/sh
cabal configure --enable-tests && cabal build && cabal test
