## Copyright (C) 2018-2022 Matthew Fluet
 # Copyright (C) 2005-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

NAME := mlnlffigen

all:	$(NAME)

$(NAME): $(shell $(MLBDEPS) $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS) -target $(TARGET) $(NAME).mlb

$(eval $(MK_COMMON_GOALS))

cppcmd.sml : gen-cppcmd
	./gen-cppcmd > $@
