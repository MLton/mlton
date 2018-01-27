## Copyright (C) 2018 Matthew Fluet
 # Copyright (C) 2005-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
LIB := $(BUILD)/lib/mlton
MLTON := mlton
TARGET := self
NAME := mlnlffigen
PATH := $(BIN):$(shell echo $$PATH)

MLTON_FLAGS := @MLton $(MLTON_RUNTIME_ARGS) --
MLTON_FLAGS += -default-ann 'sequenceNonUnit warn'
MLTON_FLAGS += $(MLTON_COMPILE_ARGS)
MLTON_FLAGS += -target $(TARGET)

all:	$(NAME)

cppcmd.sml : gen-cppcmd
	./gen-cppcmd > $@

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(MLTON_FLAGS) $(NAME).mlb

.PHONY: clean
clean:
	../bin/clean
