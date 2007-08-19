## Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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
FLAGS := -target $(TARGET)	\
	-default-ann 'sequenceNonUnit warn' \
	-default-ann 'warnUnused true'
NAME := benchmark
PATH := $(BIN):$(shell echo $$PATH)

all: 	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH="$(BIN):$$PATH" && "$(MLTON)" -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(MLTON)" $(FLAGS) $(NAME).mlb

$(NAME).sml: $(NAME).cm $(shell "$(MLTON)" -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY: clean
clean:
	../bin/clean

BENCH := barnes-hut boyer checksum count-graphs DLXSimulator fft fib flat-array hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc model-elimination mpuz nucleic output1 peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern
FPBENCH := barnes-hut fft hamlet mandelbrot matrix-multiply nucleic ray raytrace simple tensor tsp tyan vliw zern

BFLAGS := -mlton "/usr/bin/mlton" -mlton "mlton -optimize-ssa {false,true}"
BFLAGS := -wiki -mlton "/usr/bin/mlton" -mlkit -mosml -poly -smlnj
BFLAGS := -mlton "$(BIN)/mlton"

.PHONY: test
test: $(NAME)
	cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH := $(BENCH)
QBFLAGS := -mlton "~/devel/mlton/mlton-20061119-1.x86-darwin/build/bin/mlton"
QBFLAGS += -mlton "~/devel/mlton/mlton.svn.trunk/build/bin/mlton"
# QBFLAGS += -mlton "~/devel/mlton/mlton.svn.trunk.inline/build/bin/mlton -drop-pass introduceLoops1 -drop-pass loopInvariant1 -inline-leafa-loops true -inline-leafa-repeat false -inline-leafa-size 20 -drop-pass inlineLeaf2"
# QBFLAGS += -mlton "~/devel/mlton/mlton.svn.trunk.inline/build/bin/mlton -inline-leafa-loops true -inline-leafa-repeat true -inline-leafa-size 20 -inline-leafb-loops true -inline-leafb-repeat true -inline-leafb-size 40"
QBFLAGS += -mlton "~/devel/mlton/mlton.svn.trunk.inline/build/bin/mlton"

.PHONY: qtest
qtest: $(NAME)
	cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && $(MAKE) clean
