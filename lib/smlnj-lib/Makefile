## Copyright (C) 2009,2016 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: smlnj-lib.tgz smlnj-lib.patch
	rm -rf smlnj-lib
	tar -x -z --exclude '*/._*' -f smlnj-lib.tgz
	chmod -R a+r smlnj-lib
	chmod -R g-s smlnj-lib
	patch -s -d smlnj-lib -p1 < smlnj-lib.patch
	mv smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv smlnj-lib smlnj-lib-mlton
	tar -x -z --exclude '*/._*' -f smlnj-lib.tgz
	diff -Naur smlnj-lib smlnj-lib-mlton >smlnj-lib.patch || exit 0
	rm -rf smlnj-lib
	mv smlnj-lib-mlton smlnj-lib
