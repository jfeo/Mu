#!/bin/sh
rm -rf obj
ghc src/*.hs -odir obj/ -hidir obj/ -o mu -Wall -threaded
