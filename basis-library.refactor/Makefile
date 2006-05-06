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
TARGET_ARCH = x86 amd64 
TARGET_OS = linux 
DEFAULT_CHAR = char8
DEFAULT_INT = int32 int64 intinf
DEFAULT_REAL = real32 real64
DEFAULT_WORD = word32 word64

.PHONY: type-check-def
type-check-def:
	$(MLTON) -disable-ann deadCode -stop tc -show-types true \
		libs/all.mlb; \

.PHONY: type-check-all
type-check-all:
	for objptrrep in $(OBJPTR_MAPS); do \
	for header in $(HEADER_MAPS); do \
	for seqindex in $(SEQINDEX_MAPS); do \
	for targetarch in $(TARGET_ARCH); do \
	for targetos in $(TARGET_OS); do \
	for defchar in $(DEFAULT_CHAR); do \
	for defint in $(DEFAULT_INT); do \
	for defreal in $(DEFAULT_REAL); do \
	for defword in $(DEFAULT_WORD); do \
	if [ ! -r config/c/$$targetarch-$$targetos/c-types.sml ]; then \
		break; \
	fi; \
	echo "Type checking: $$objptrrep $$header $$seqindex $$targetarch $$targetos $$defchar $$defint $$defreal $$defword"; \
	$(MLTON) -disable-ann deadCode -stop tc -show-types true \
		-mlb-path-map "maps/$$objptrrep" \
		-mlb-path-map "maps/$$header" \
		-mlb-path-map "maps/$$seqindex" \
		-mlb-path-map "maps/c-types.$$targetarch-$$targetos.map" \
		-default-type "$$defchar" \
		-default-type "$$defint" \
		-default-type "$$defreal" \
		-default-type "$$defword" \
		libs/all.mlb; \
	done; done; done; done; done; done; done; done; done
