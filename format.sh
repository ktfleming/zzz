#!/usr/local/bin/zsh

set -e

ormolu --mode inplace app/Main.hs 
ormolu --mode inplace src/**/*.hs
ormolu --mode inplace test/**/*.hs
