## Copyright (C) 2009,2016 Matthew Fluet.
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
	tar -x -z --exclude '*/._*' -f MLRISC.tgz
	chmod -R a+r MLRISC
	chmod -R g-s MLRISC
	patch -s -d MLRISC -p1 < MLRISC.patch

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv MLRISC MLRISC-mlton
	tar -x -z --exclude '*/._*' -f MLRISC.tgz
	diff -Naur MLRISC MLRISC-mlton >MLRISC.patch || exit 0
	rm -rf MLRISC
	mv MLRISC-mlton MLRISC
