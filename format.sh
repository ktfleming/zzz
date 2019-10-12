#!/usr/local/bin/zsh

stylish-haskell -i app/Main.hs && brittany --write-mode=inplace app/Main.hs && stylish-haskell -i src/**/*.hs && brittany --write-mode=inplace src/**/*.hs
