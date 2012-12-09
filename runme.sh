#!/bin/sh

echo "[cgrep] compiling..."

/usr/bin/ghc -Wall -O3 Main.hs -o cgrep -threaded -with-rtsopts="-N" 
