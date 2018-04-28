## Copyright (C) 2010,2013,2016-2017 Matthew Fluet.
 # Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
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
	../bin/clean


RESOLVE_SCOPES = program topdec strdec dec
OBJPTR_MAPS = objptr-rep32.map objptr-rep64.map
NORMAL_METADATA_MAPS = normal-metadata-size32.map normal-metadata-size64.map normal-metadata-size128.map
ARRAY_METADATA_MAPS = array-metadata-size96.map array-metadata-size128.map array-metadata-size196.map array-metadata-size256.map
SEQINDEX_MAPS = seqindex-int32.map seqindex-int64.map 
DEFAULT_CHAR = char8
DEFAULT_INT = int32 int64 intinf
DEFAULT_REAL = real32 real64
DEFAULT_WORD = word32 word64

.PHONY: def-use
def-use:
	"$(MLTON)" -disable-ann deadCode -stop tc -show-types true \
		-prefer-abs-paths true -show-def-use basis-library.def-use \
		libs/all.mlb

.PHONY: type-check-def
type-check-def:
	"$(MLTON)" -disable-ann deadCode -stop tc -show-types true \
		libs/all.mlb

.PHONY: type-check-all
type-check-all:
	for resolvescope in $(RESOLVE_SCOPES); do \
	for objptr in $(OBJPTR_MAPS); do \
	for normalmetadata in $(NORMAL_METADATA_MAPS); do \
	for arraymetadata in $(ARRAY_METADATA_MAPS); do \
	for seqindex in $(SEQINDEX_MAPS); do \
	for defchar in $(DEFAULT_CHAR); do \
	for defint in $(DEFAULT_INT); do \
	for defreal in $(DEFAULT_REAL); do \
	for defword in $(DEFAULT_WORD); do \
	echo "Type checking: $$resolvescope  $$objptr $$normalmetadata $$arraymetadata $$seqindex  $$defchar $$defint $$defreal $$defword"; \
	echo "$(MLTON)" -disable-ann deadCode -disable-ann resolveScope -default-ann "\"resolveScope $$resolvescope\"" -stop tc -show-types true \
		-mlb-path-map "maps/$$objptr" \
		-mlb-path-map "maps/$$normalmetadata" \
		-mlb-path-map "maps/$$arraymetadata" \
		-mlb-path-map "maps/$$seqindex" \
		-default-type "$$defchar" \
		-default-type "$$defint" \
		-default-type "$$defreal" \
		-default-type "$$defword" \
		libs/all.mlb; \
	"$(MLTON)" -disable-ann deadCode -disable-ann resolveScope -default-ann "resolveScope $$resolvescope" -stop tc -show-types true \
		-mlb-path-map "maps/$$objptr" \
		-mlb-path-map "maps/$$normalmetadata" \
		-mlb-path-map "maps/$$arraymetadata" \
		-mlb-path-map "maps/$$seqindex" \
		-default-type "$$defchar" \
		-default-type "$$defint" \
		-default-type "$$defreal" \
		-default-type "$$defword" \
		libs/all.mlb; \
	done; done; done; done; done; done; done; done; done;
