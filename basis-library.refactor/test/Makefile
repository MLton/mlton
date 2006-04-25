## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC = $(shell cd ../.. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
MLTON = mlton
PATH = $(BIN):$(shell echo $$PATH)

all: test

.PHONY: clean
clean:
	find . -type f | egrep '.(old|ast|core-ml)$$' | xargs rm -f
	../bin/clean


OBJPTR_REP_MAPS = objptr-rep32.map
HEADER_MAPS = header-word32.map
SEQ_INDEX_MAPS = seqindex-int32.map
CTYPES_MAPS = c-types.m32.map
DEFAULT_CHAR_MAPS = default-char8.map
DEFAULT_INT_MAPS = default-int32.map default-int64.map default-intinf.map 
DEFAULT_REAL_MAPS = default-real64.map
DEFAULT_WORD_MAPS = default-word32.map default-word64.map

test: test.mlb print.o $(shell $(MLTON) -mlb-path-map "../maps/header-word32.map" -mlb-path-map "../maps/objptr-rep32.map" -mlb-path-map "../maps/seqindex-int32.map" -mlb-path-map "../maps/c-types.m32.map" -mlb-path-map "../maps/default-char8.map" -mlb-path-map "../maps/default-int32.map" -mlb-path-map "../maps/default-real64.map" -mlb-path-map "../maps/default-word32.map" -stop f test.mlb)
	$(MLTON) \
		-mlb-path-map "../maps/header-word32.map" \
		-mlb-path-map "../maps/objptr-rep32.map" \
		-mlb-path-map "../maps/seqindex-int32.map" \
		-mlb-path-map "../maps/c-types.m32.map" \
		-mlb-path-map "../maps/default-char8.map" \
		-mlb-path-map "../maps/default-int32.map" \
		-mlb-path-map "../maps/default-real64.map" \
		-mlb-path-map "../maps/default-word32.map" \
		-codegen c \
		-const 'Exn.keepHistory true' \
		-profile-include '<basis>' \
		-profile-branch true \
		-profile-raise true \
		test.mlb print.o

print.o: print.c
	gcc -m32 -c print.c
