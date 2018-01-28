## Copyright (C) 2009,2016,2018 Matthew Fluet.
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
	tar -x -z -f smlnj-lib.tgz
	find smlnj-lib -name '._*' -exec rm -f '{}' ';'
	chmod -R a+r smlnj-lib
	chmod -R g-s smlnj-lib
	patch -s -d smlnj-lib -p1 < smlnj-lib.patch
	mv smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE
	touch $@

.PHONY: clean
clean:
	../../bin/clean
