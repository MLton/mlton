## Copyright (C) 2022 Matthew Fluet
 # Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../../..
include $(ROOT)/Makefile.config

######################################################################

PATH  := $(BIN):$(shell echo $$PATH)
mlton := mlton -default-ann 'allowFFI true'

all: test

.PHONY: test
test: finalizable
	./finalizable

finalizable: finalizable.sml cons.c
	$(mlton) finalizable.sml cons.c


$(eval $(MK_COMMON_GOALS))
