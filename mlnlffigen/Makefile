## Copyright (C) 2018-2019 Matthew Fluet
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

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(MLTON)" @MLton $(MLTON_RUNTIME_ARGS) -- $(MLTON_COMPILE_ARGS) -target $(TARGET) $(NAME).mlb

.PHONY: clean
clean:
	../bin/clean


cppcmd.sml : gen-cppcmd
	./gen-cppcmd > $@
