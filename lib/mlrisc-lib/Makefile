## Copyright (C) 2009,2016,2018 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

FIND := find
GZIP := gzip
PATCH := patch
TAR := tar

######################################################################
######################################################################

all: MLRISC/README.mlton

MLRISC/README.mlton: MLRISC.tgz MLRISC.patch
	rm -rf MLRISC
	$(GZIP) -dc MLRISC.tgz | $(TAR) xf -
	$(FIND) MLRISC -name '._*' -depth -exec rm -f '{}' ';'
	chmod -R a+r MLRISC
	chmod -R g-s MLRISC
	$(PATCH) -s -d MLRISC -p1 < MLRISC.patch

.PHONY: clean
clean:
	../../bin/clean
