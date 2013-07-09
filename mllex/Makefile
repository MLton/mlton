## Copyright (C) 2009,2013 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
LIB := $(BUILD)/lib
MLTON := mlton
TARGET := self
FLAGS := -target $(TARGET)
NAME := mllex
PATH := $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(MLTON)" $(FLAGS) $(NAME).mlb

html/index.html: $(TEX_FILES)
	mkdir -p html
	hevea -fix -o html/mllex.html -exec xxdate.exe macros.hva lexgen.tex
	cd html && hacha mllex.html && rm -f mllex.html

PDFLATEX := pdflatex

lexgen.pdf: lexgen.tex
	$(PDFLATEX) lexgen.tex
	$(PDFLATEX) lexgen.tex

mllex.pdf: lexgen.pdf
	cp lexgen.pdf mllex.pdf

DOCS :=
ifneq ($(shell which $(PDFLATEX)),)
DOCS += mllex.pdf
endif

.PHONY: docs
docs: $(DOCS)

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.lex . &&			\
	$(NAME) ml.lex &&					\
	diff ml.lex.sml ../mlton/front-end/ml.lex.sml
