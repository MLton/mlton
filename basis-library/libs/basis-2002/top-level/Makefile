## Copyright (C) 2022 Matthew Fluet.
 # Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../../../..
include $(ROOT)/Makefile.config

######################################################################

GEN := generate-overloads

overloads.sml: $(GEN).sml
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS)  $(GEN).sml
	$(GEN) >overloads.sml

$(eval $(MK_COMMON_GOALS))
