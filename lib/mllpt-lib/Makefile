## Copyright (C) 2011,2016,2018,2020,2022 Matthew Fluet.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################
######################################################################

all: ml-lpt/README

ml-lpt/README: ml-lpt.tgz ml-lpt.patch
	$(RM) ml-lpt
	$(GZIP) -dc ml-lpt.tgz | $(TAR) xf -
	$(FIND) ml-lpt -name '._*' -prune -exec $(RM) '{}' ';'
	$(CHMOD) -R a+r ml-lpt
	$(CHMOD) -R g-s ml-lpt
	$(PATCH) -s -d ml-lpt -p1 < ml-lpt.patch
	touch $@  # ml-lpt/README not created/updated by patch

$(eval $(MK_COMMON_GOALS))
