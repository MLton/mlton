## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
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

$(NAME).sml: $(NAME).cm $(shell "$(MLTON)" -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

html/index.html: $(TEX_FILES)
	mkdir -p html
	hevea -fix -o html/mllex.html -exec xxdate.exe macros.hva lexgen.tex
	cd html && hacha mllex.html && rm -f mllex.html

lexgen.dvi: lexgen.tex
	latex lexgen.tex
	latex lexgen.tex

mllex.pdf: lexgen.tex
	latex lexgen.tex
	pdflatex lexgen.tex
	mv lexgen.pdf mllex.pdf

mllex.ps: lexgen.dvi
	dvips -o mllex.ps lexgen.dvi

.PHONY: clean
clean:
	../bin/clean

.PHONY: docs
docs: mllex.pdf

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.lex . &&			\
	$(NAME) ml.lex &&					\
	diff ml.lex.sml ../mlton/front-end/ml.lex.sml
