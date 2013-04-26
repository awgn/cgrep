# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

# GHCFLAGS= --make -O2 -Wall -threaded -rtsopts -with-rtsopts="-A1G -N" -eventlog # -with-rtsopts="-N" 
GHCFLAGS= --make -ddump-splices -O2 -Wall -threaded -rtsopts -with-rtsopts="-A1G -N" # -prof -auto-all 

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean install cgrep

all: cgrep 

SRC = Main.hs


cgrep: $(SRC) 
		@mkdir -p bin/
		$(HC) $(GHCFLAGS) $< -o bin/$@

install: all
		cp bin/cgrep ${INSTDIR}/bin/

clean:
	   @rm -f bin/cgrep
	   @rm -f *.o *.hi CGrep/*.o CGrep/*.hi CGrep/Cpp/*.o CGrep/Cpp/*.hi CGrep/Strategy/*.o CGrep/Strategy/*.hi
