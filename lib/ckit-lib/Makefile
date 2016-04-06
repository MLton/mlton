## Copyright (C) 2009,2016 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: ckit/README.mlton

ckit/README.mlton: ckit.tgz ckit.patch
	rm -rf ckit
	tar -x -z --exclude '*/._*' -f ckit.tgz
	chmod -R a+r ckit
	chmod -R g-s ckit
	chmod a-x ckit/src/ast/simplify-assign-ops.sml
	patch -s -d ckit -p1 < ckit.patch

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv ckit ckit-mlton
	tar -x -z --exclude '*/._*' -f ckit.tgz
	diff -Naur ckit ckit-mlton >ckit.patch || exit 0
	rm -rf ckit
	mv ckit-mlton ckit
