## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: ckit/README.mlton

ckit/README.mlton: ckit.tgz ckit.patch
	rm -rf ckit
	gzip -dc ckit.tgz | tar xf -
	chmod -R a+r ckit
	chmod -R g-s ckit
	chmod a-x ckit/src/ast/simplify-assign-ops.sml
	patch -s -p0 < ckit.patch

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv ckit ckit-mlton
	gzip -dc ckit.tgz | tar xf -
	diff -Naur ckit ckit-mlton >ckit.patch || exit 0
	rm -rf ckit
	mv ckit-mlton ckit
