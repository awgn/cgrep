# (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
#

# Using hint generates a large binary...
# GHCFLAGS= --make -O3 -Wall -threaded -rtsopts -with-rtsopts="-A64M -N" -cpp -DENABLE_HINT

# Light version
GHCFLAGS= --make -O2 -Wall -threaded -rtsopts -with-rtsopts="-A64M -N" -cpp


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
		cp ../cgreprc 	${HOME}/.cgreprc

clean:
	   @rm -f ../bin/cgrep
	   @rm -f *.o *.hi */*.o */*/*.o */*/*/*.o */*.hi */*/*.hi */*/*/*.hi
