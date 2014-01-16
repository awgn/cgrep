# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

#GHCFLAGS= --make -O2 -Wall -threaded -rtsopts -eventlog # -with-rtsopts="-N" 
#GHCFLAGS= --make -ddump-splices -O3 -Wall -threaded -rtsopts -with-rtsopts="-A64M -H16M -N" #  -prof -auto-all 

# GHCFLAGS= --make -O3 -Wall -threaded -rtsopts -with-rtsopts="-A64M -N" -cpp -DENABLE_HINT
GHCFLAGS= --make -O3 -Wall -threaded -rtsopts -with-rtsopts="-A64M -N" -cpp # light version


INSTDIR=/usr/local

HC=ghc

.PHONY: all clean install cgrep

all: cgrep 

SRC = Main.hs


cgrep: $(SRC) 
		@mkdir -p ../bin/
		$(HC) $(GHCFLAGS) $< -o ../bin/$@

install: all
		cp ../bin/cgrep ${INSTDIR}/bin/

clean:
	   @rm -f ../bin/cgrep
	   @rm -f *.o *.hi CGrep/*.o CGrep/*.hi CGrep/Strategy/Cpp/*.o CGrep/Strategy/Cpp/*.hi CGrep/Strategy/*.o CGrep/Strategy/*.hi
