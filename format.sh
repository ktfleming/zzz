#!/bin/sh

# Sort imports
stylish-haskell -i src/**/*.hs

# Format everything else
brittany --write-mode=inplace src/**/*.hs

