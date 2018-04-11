#!/bin/sh

# Watch hs files in src, and run test script for the file that changed (filename without extension)
fswatch -e ".*" -i "\\.hs$" -0r src | xargs -0 -I{} sh -c 'echo; echo; echo "*** Changed: {} ***"; echo; sh runTests.sh $(basename {} .hs)' 
