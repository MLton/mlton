## Copyright (C) 2009,2016,2018,2020 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

all: ckit/README.mlton

ckit/README.mlton: ckit.tgz ckit.patch
	$(RM) ckit
	$(GZIP) -dc ckit.tgz | $(TAR) xf -
	$(FIND) ckit -name '._*' -prune -exec $(RM) '{}' ';'
	chmod -R a+r ckit
	chmod -R g-s ckit
	chmod a-x ckit/src/ast/simplify-assign-ops.sml
	$(PATCH) -s -d ckit -p1 < ckit.patch

.PHONY: clean
clean:
	../../bin/clean
