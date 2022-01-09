## Copyright (C) 2022 Matthew Fluet
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

all:

CLEAN_EXES := true
CLEAN_EXTS := c s S
$(eval $(MK_COMMON_GOALS))

.PHONY: abnormal
abnormal:
	ls -1 | egrep -v '\.ok$$|\.sml$$'
