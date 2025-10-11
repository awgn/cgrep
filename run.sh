#!/bin/bash 

cgrep.7.1.0 $@ | wc -l
cgrep $@ | wc -l
