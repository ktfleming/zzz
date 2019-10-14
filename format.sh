#!/usr/local/bin/zsh

ormolu --mode inplace app/Main.hs && \
ormolu --mode inplace src/**/*.hs && \
ormolu --mode inplace test/**/*.hs &&
