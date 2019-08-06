#!/bin/sh

# First run brittany for the main formatting
brittany --write-mode=inplace **/*.hs

# Then run stylish-haskell, which only formats/sorts the imports
# (overriding what brittany did)
stylish-haskell -i **/*.hs

