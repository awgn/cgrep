# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

GHCFLAGS= --make -O2 -Wall -threaded -with-rtsopts="-N" 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: cgrep 

cgrep: Main.hs Cgrep.hs CgrepSimple.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		cp cgrep  ${INSTDIR}/bin/
        
clean:
	   @rm -f cgrep
	   @rm -f *.o *.hi
