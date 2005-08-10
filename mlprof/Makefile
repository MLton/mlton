## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
TARGET = self
FLAGS = -target $(TARGET) -default-ann 'sequenceNonUnit warn' -default-ann 'warnUnused true'
NAME = mlprof
PATH = $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH=$(BIN):$$PATH && $(MLTON) -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(FLAGS) $(NAME).mlb
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	./mlprof z mlmon.out
