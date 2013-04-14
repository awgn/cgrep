# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

# GHCFLAGS= --make -O2 -Wall -threaded -rtsopts -with-rtsopts="-A1G -N" -eventlog # -with-rtsopts="-N" 
GHCFLAGS= --make -O2 -Wall -threaded -rtsopts -with-rtsopts="-A1G -N"  

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: cgrep-new 

SRC = Main.hs


cgrep-new: $(SRC) 
		$(HC) $(GHCFLAGS) $< -o $@

install: all
		cp cgrep-new  ${INSTDIR}/bin/

clean:
	   @rm -f cgrep-new
	   @rm -f *.o *.hi CGrep/*.o CGrep/*.hi
