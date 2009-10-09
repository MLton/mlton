## Copyright (C) 2009 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC := $(shell cd ../.. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
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

.PHONY: all
all: ml.lex.sml ml.grm.sig ml.grm.sml mlb.lex.sml mlb.grm.sig mlb.grm.sml

.PHONY: clean
clean:
	../../bin/clean

ml.lex.sml: ml.lex
	rm -f ml.lex.sml
	$(MLLEX) ml.lex
	chmod -w ml.lex.sml

ml.grm.sig ml.grm.sml: ml.grm
	rm -f ml.grm.*
	$(MLYACC) ml.grm
	chmod -w ml.grm.*

mlb.lex.sml: mlb.lex
	rm -f mlb.lex.sml
	$(MLLEX) mlb.lex
	chmod -w mlb.lex.sml

mlb.grm.sig mlb.grm.sml: mlb.grm
	rm -f mlb.grm.*
	$(MLYACC) mlb.grm
	chmod -w mlb.grm.*
