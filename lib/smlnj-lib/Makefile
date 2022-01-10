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

all: smlnj-lib/README.mlton

smlnj-lib/README.mlton: smlnj-lib.tgz smlnj-lib.patch
	$(RM) smlnj-lib
	$(GZIP) -dc smlnj-lib.tgz | $(TAR) xf -
	$(FIND) smlnj-lib -name '._*' -prune -exec $(RM) '{}' ';'
	$(CHMOD) -R a+r smlnj-lib
	$(CHMOD) -R g-s smlnj-lib
	$(PATCH) -s -d smlnj-lib -p1 < smlnj-lib.patch
	$(CP) smlnj-lib/LICENSE ../../doc/license/SMLNJ-LIB-LICENSE

$(eval $(MK_COMMON_GOALS))
