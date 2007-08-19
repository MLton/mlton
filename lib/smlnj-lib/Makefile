## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: smlnj-lib.tgz smlnj-lib.patch
	rm -rf smlnj-lib
	gzip -dc smlnj-lib.tgz | tar xf -
	chmod -R a+r smlnj-lib
	chmod -R g-s smlnj-lib
	rm -f smlnj-lib/HTML/.cvsignore
	patch -s -p0 < smlnj-lib.patch
	mv smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv smlnj-lib smlnj-lib-mlton
	gzip -dc smlnj-lib.tgz | tar xf -
	diff -Naur smlnj-lib smlnj-lib-mlton >smlnj-lib.patch || exit 0
	rm -rf smlnj-lib
	mv smlnj-lib-mlton smlnj-lib
