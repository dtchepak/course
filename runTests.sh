#!/bin/sh
if [ $# -eq 1 ]
then
    cabal test tasty --show-detail=direct --test-option=--pattern=Tests/$1/
else
    cabal test tasty --show-detail=direct
fi
