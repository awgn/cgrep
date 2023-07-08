#!/bin/bash
rm cgrep.prof
stack build --profile -v
echo "running: cgrep ${@}..." 
stack exec --profile -- cgrep $@ +RTS -P | wc -l
profiteur cgrep.prof
open cgrep.prof.html
cat cgrep.prof
