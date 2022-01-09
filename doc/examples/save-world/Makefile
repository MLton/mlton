## Copyright (C) 2022 Matthew Fluet
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../../..
include $(ROOT)/Makefile.config

######################################################################

PATH  := $(BIN):$(shell echo $$PATH)
mlton := mlton

all: test

.PHONY: test
test: world
	./save-world @MLton load-world world --

world: save-world
	./save-world

save-world: save-world.sml
	$(mlton) save-world.sml


$(eval $(MK_COMMON_GOALS))
