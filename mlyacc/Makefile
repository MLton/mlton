## Copyright (C) 2009,2013,2018 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

MLTON_RUNTIME_ARGS :=
MLTON_COMPILE_ARGS :=

DIFF := diff

######################################################################
######################################################################

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin

PATH := $(BIN):$(shell echo $$PATH)

TARGET := self

######################################################################

MLTON := mlton
NAME := mlyacc

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(MLTON)" @MLton $(MLTON_RUNTIME_ARGS) -- $(MLTON_COMPILE_ARGS) -target $(TARGET) $(NAME).mlb

.PHONY: clean
clean:
	../bin/clean


ifeq (mllex, $(shell if mllex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo mllex; fi))
MLLEX := mllex
else
ifeq (ml-lex, $(shell if ml-lex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo ml-lex; fi))
MLLEX := ml-lex
else
MLLEX := no-mllex
endif
endif

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		$(MLLEX) src/yacc.lex && \
		chmod -w src/yacc.lex.sml


ifeq (mlyacc, $(shell if mlyacc >/dev/null 2>&1 || [ $$? != 127 ] ; then echo mlyacc; fi))
MLYACC := mlyacc
else
ifeq (ml-yacc, $(shell if ml-lex >/dev/null 2>&1 || [ $$? != 127 ] ; then echo ml-yacc; fi))
MLYACC := ml-yacc
else
MLYACC := no-mlyacc
endif
endif

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


.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm .			\
	$(NAME) ml.grm &&					\
	$(DIFF) ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	$(DIFF) ml.grm.sml ../mlton/front-end/ml.grm.sml	\
	rm -f ml.grm ml.grm.sig ml.grm.sml ml.grm.desc
