## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: smlnj-lib.tgz smlnj-lib.patch
	gzip -dc smlnj-lib.tgz | tar xf -
	chmod -R a+r smlnj-lib
	patch -p0 < smlnj-lib.patch
	mv smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE

.PHONY: clean
clean:
	../../bin/clean
