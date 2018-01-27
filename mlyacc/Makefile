## Copyright (C) 2009,2013,2018 Matthew Fluet.
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
LIB := $(BUILD)/lib/mlton
MLTON := mlton
TARGET := self
NAME := mlyacc
PATH := $(BIN):$(shell echo $$PATH)

MLTON_FLAGS := @MLton $(MLTON_RUNTIME_ARGS) --
MLTON_FLAGS += $(MLTON_COMPILE_ARGS)
MLTON_FLAGS += -target $(TARGET)

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
	"$(MLTON)" $(MLTON_FLAGS) $(NAME).mlb

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		$(MLLEX) src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/%.grm.sig src/%.grm.sml: src/%.grm
	rm -f $<.* 
	$(MLYACC) $<
	chmod -w $<.*

PDFLATEX := pdflatex

doc/mlyacc.pdf:
	$(MAKE) -C doc mlyacc.pdf

mlyacc.pdf: doc/mlyacc.pdf
	cp doc/mlyacc.pdf .

DOCS :=
ifneq ($(shell which $(PDFLATEX) 2> /dev/null),)
DOCS += mlyacc.pdf
endif

.PHONY: docs
docs: $(DOCS)

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm . &&			\
	$(NAME) ml.grm &&					\
	diff ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	diff ml.grm.sml ../mlton/front-end/ml.grm.sml
