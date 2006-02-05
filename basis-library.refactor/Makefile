## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
MLTON = mlton
PATH = $(BIN):$(shell echo $$PATH)

all:

.PHONY: clean
clean:
	find . -type f | egrep '.(old|ast|core-ml)$$' | xargs rm -f
	../bin/clean


CTYPES_MAPS = c-types.m32.map c-types.m64.map c-types.weird.map
DEFAULT_CHAR_MAPS = default-char8.map
DEFAULT_INT_MAPS = default-int32.map default-int64.map default-intinf.map 
DEFAULT_WORD_MAPS = default-word32.map default-word64.map
OBJPTR_REP_MAPS = objptr-rep32.map objptr-rep64.map 
SEQ_INDEX_MAPS = seq-index32.map seq-index64.map 

.PHONY: type-check
type-check:
	for ctypes in $(CTYPES_MAPS); do \
	for defchar in $(DEFAULT_CHAR_MAPS); do \
	for defint in $(DEFAULT_INT_MAPS); do \
	for defword in $(DEFAULT_WORD_MAPS); do \
	for objptrrep in $(OBJPTR_REP_MAPS); do \
	for seqindex in $(SEQ_INDEX_MAPS); do \
	echo "Type checking: $$ctypes $$defchar $$defint $$defword $$objptrrep $$seqindex"; \
	$(MLTON) -disable-ann deadCode -stop tc -show-types true \
		-mlb-path-map "maps/$$ctypes" \
		-mlb-path-map "maps/$$defchar" \
		-mlb-path-map "maps/$$defint" \
		-mlb-path-map "maps/$$defword" \
		-mlb-path-map "maps/$$objptrrep" \
		-mlb-path-map "maps/$$seqindex" \
		build/sources.mlb; \
	done; done; done; done; done; done
