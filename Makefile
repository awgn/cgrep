# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

GHCFLAGS= --make -O2 -Wall 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: cgrep 

cgrep: cgrep.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		cp cgrep  ${INSTDIR}/bin/
        
clean:
	   @rm -f cgrep
	   @rm -f *.o *.hi
