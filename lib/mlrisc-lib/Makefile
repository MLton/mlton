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

all: MLRISC/README.mlton

MLRISC/README.mlton: MLRISC.tgz MLRISC.patch
	$(RM) MLRISC
	$(GZIP) -dc MLRISC.tgz | $(TAR) xf -
	$(FIND) MLRISC -name '._*' -prune -exec $(RM) '{}' ';'
	$(CHMOD) -R a+r MLRISC
	$(CHMOD) -R g-s MLRISC
	$(PATCH) -s -d MLRISC -p1 < MLRISC.patch

$(eval $(MK_COMMON_GOALS))
