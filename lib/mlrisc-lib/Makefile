## Copyright (C) 2009,2016,2018 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: MLRISC/README.mlton

MLRISC/README.mlton: MLRISC.tgz MLRISC.patch
	rm -rf MLRISC
	tar -x -z -f MLRISC.tgz
	find MLRISC -name '._*' -exec rm -f '{}' ';'
	chmod -R a+r MLRISC
	chmod -R g-s MLRISC
	patch -s -d MLRISC -p1 < MLRISC.patch
	touch $@

.PHONY: clean
clean:
	../../bin/clean
