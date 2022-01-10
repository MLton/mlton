## Copyright (C) 2009,2016,2018,2020,2022 Matthew Fluet.
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
	$(CHMOD) -R a+r ckit
	$(CHMOD) -R g-s ckit
	$(CHMOD) a-x ckit/src/ast/simplify-assign-ops.sml
	$(PATCH) -s -d ckit -p1 < ckit.patch

$(eval $(MK_COMMON_GOALS))
