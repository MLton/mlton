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

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: smlnj-lib.tgz smlnj-lib.patch
	rm -rf smlnj-lib
	$(GZIP) -dc smlnj-lib.tgz | $(TAR) xf -
	$(FIND) smlnj-lib -name '._*' -prune -exec rm -f '{}' ';'
	chmod -R a+r smlnj-lib
	chmod -R g-s smlnj-lib
	$(PATCH) -s -d smlnj-lib -p1 < smlnj-lib.patch
	cp smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE

.PHONY: clean
clean:
	../../bin/clean
