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
	chmod -R a+r ckit
	patch -p0 < ckit.patch

.PHONY: clean
clean:
	../../bin/clean
