## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: 
	gzip -dc smlnj-lib.tgz | tar xf -
	patch -p0 < smlnj-lib.patch	

.PHONY: clean
clean:
	../../bin/clean
