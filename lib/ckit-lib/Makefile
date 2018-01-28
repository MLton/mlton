## Copyright (C) 2009,2016,2018 Matthew Fluet.
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
	tar -x -z -f ckit.tgz
	find ckit -name '._*' -exec rm -f '{}' ';'
	chmod -R a+r ckit
	chmod -R g-s ckit
	echo chmod a-x ckit/src/ast/simplify-assign-ops.sml
	patch -s -d ckit -p1 < ckit.patch

.PHONY: clean
clean:
	../../bin/clean
