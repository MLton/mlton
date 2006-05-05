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


OBJPTR_MAPS = objptr-rep32.map objptr-rep64.map 
HEADER_MAPS = header-word32.map header-word64.map
SEQINDEX_MAPS = seqindex-int32.map seqindex-int64.map 
CTYPES_MAPS = c-types.test-amd64-m32-linux.map c-types.test-amd64-m64-linux.map c-types.test-weird-weird.map c-types.amd64-linux.map c-types.x86-linux.map 
DEFAULT_CHAR_MAPS = default-char8.map
DEFAULT_INT_MAPS = default-int32.map default-int64.map default-int-inf.map 
DEFAULT_REAL_MAPS = default-real32.map default-real64.map
DEFAULT_WORD_MAPS = default-word32.map default-word64.map

.PHONY: type-check-one
type-check-one:
	for objptrrep in objptr-rep32.map; do \
	for header in header-word32.map; do \
	for seqindex in seqindex-int32.map; do \
	for defchar in default-char8.map; do \
	for defint in default-int32.map; do \
	for defreal in default-real64.map; do \
	for defword in default-word32.map; do \
	echo "Type checking: $$objptrrep $$header $$seqindex $$ctypes $$defchar $$defint $$defreal $$defword"; \
	$(MLTON) -disable-ann deadCode -stop tc -show-types true \
		-mlb-path-map "maps/$$objptrrep" \
		-mlb-path-map "maps/$$header" \
		-mlb-path-map "maps/$$seqindex" \
		-mlb-path-map "maps/$$defchar" \
		-mlb-path-map "maps/$$defint" \
		-mlb-path-map "maps/$$defreal" \
		-mlb-path-map "maps/$$defword" \
		libs/all.mlb; \
	done; done; done; done; done; done; done

.PHONY: type-check
type-check:
	for objptrrep in $(OBJPTR_MAPS); do \
	for header in $(HEADER_MAPS); do \
	for seqindex in $(SEQINDEX_MAPS); do \
	for ctypes in $(CTYPES_MAPS); do \
	for defchar in $(DEFAULT_CHAR_MAPS); do \
	for defint in $(DEFAULT_INT_MAPS); do \
	for defreal in $(DEFAULT_REAL_MAPS); do \
	for defword in $(DEFAULT_WORD_MAPS); do \
	echo "Type checking: $$objptrrep $$header $$seqindex $$ctypes $$defchar $$defint $$defreal $$defword"; \
	$(MLTON) -disable-ann deadCode -stop tc -show-types true \
		-mlb-path-map "maps/$$objptrrep" \
		-mlb-path-map "maps/$$header" \
		-mlb-path-map "maps/$$seqindex" \
		-mlb-path-map "maps/$$ctypes" \
		-mlb-path-map "maps/$$defchar" \
		-mlb-path-map "maps/$$defint" \
		-mlb-path-map "maps/$$defreal" \
		-mlb-path-map "maps/$$defword" \
		libs/all.mlb; \
	done; done; done; done; done; done; done; done
