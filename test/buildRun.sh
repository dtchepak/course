#!/bin/sh
rm course-test.tix && cabal-dev build && dist/build/course-test/course-test
