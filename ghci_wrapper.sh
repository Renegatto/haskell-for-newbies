#!/bin/bash

if [[ "$#" == 0 ]]; then
    exec cabal repl app/Main.hs
fi

# This is needed to run the `main` function by default.
{ echo main ; cat - ; } | exec exec cabal repl app/Main.hs "$@"