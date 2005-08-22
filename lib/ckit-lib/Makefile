## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: ckit/README.mlton

ckit/README.mlton: ckit.tgz ckit.patch
	gzip -dc ckit.tgz | tar xf -
	patch -p0 < ckit.patch
	chmod a-x ckit/src/ast/simplify-assign-ops.sml

.PHONY: clean
clean:
	../../bin/clean
