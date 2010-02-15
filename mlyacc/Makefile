## Copyright (C) 2009 Matthew Fluet.
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
NAME := mlyacc
PATH := $(BIN):$(shell echo $$PATH)

ifeq (mllex, $(shell if mllex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo mllex; fi))
MLLEX := mllex
else
ifeq (ml-lex, $(shell if ml-lex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo ml-lex; fi))
MLLEX := ml-lex
else
MLLEX := no-mllex
endif
endif

ifeq (mlyacc, $(shell if mlyacc >/dev/null 2>&1 || [ $$? != 127 ] ; then echo mlyacc; fi))
MLYACC := mlyacc
else
ifeq (ml-yacc, $(shell if ml-lex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo ml-yacc; fi))
MLYACC := ml-yacc
else
MLYACC := no-mlyacc
endif
endif

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(MLTON)" $(FLAGS) $(NAME).mlb

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		$(MLLEX) src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/%.grm.sig src/%.grm.sml: src/%.grm
	rm -f $<.* 
	$(MLYACC) $<
	chmod -w $<.*

doc/mlyacc.ps:
	$(MAKE) -C doc mlyacc.ps

doc/mlyacc.pdf:
	$(MAKE) -C doc mlyacc.pdf

mlyacc.ps: doc/mlyacc.ps
	cp doc/mlyacc.ps .

mlyacc.pdf: doc/mlyacc.pdf
	cp doc/mlyacc.pdf .

.PHONY: clean
clean:
	../bin/clean

.PHONY: docs
docs: mlyacc.pdf

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm . &&			\
	$(NAME) ml.grm &&					\
	diff ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	diff ml.grm.sml ../mlton/front-end/ml.grm.sml
