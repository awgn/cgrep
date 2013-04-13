# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

GHCFLAGS= --make -O2 -Wall -threaded -with-rtsopts="-N" 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: cgrep-new 

cgrep-new: Main.hs Cgrep.hs CgrepSimple.hs
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		cp cgrep-new  ${INSTDIR}/bin/
        
clean:
	   @rm -f cgrep-new
	   @rm -f *.o *.hi
