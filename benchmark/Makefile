## Copyright (C) 2013 Matthew Fluet.
 # Copyright (C) 2009 Matthew Fluet.
 # Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
LIB := $(BUILD)/lib/mlton
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

.PHONY: clean
clean:
	../bin/clean

BENCH := barnes-hut boyer checksum count-graphs DLXSimulator even-odd fft fib flat-array hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc model-elimination mpuz nucleic output1 peek psdes-random ratio-regions ray raytrace simple smith-normal-form string-concat tailfib tak tensor tsp tyan vector32-concat vector64-concat vector-rev vliw wc-input1 wc-scanStream zebra zern
FPBENCH := barnes-hut fft hamlet mandelbrot matrix-multiply nucleic ray raytrace simple tensor tsp tyan vliw zern

BFLAGS := -mlton "/usr/bin/mlton" -mlton "mlton -optimize-ssa {false,true}"
BFLAGS := -wiki -mlton "/usr/bin/mlton" -mlkit -mosml -poly -smlnj
BFLAGS := -mlton "$(BIN)/mlton"

.PHONY: test
test: $(NAME)
	cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH := $(BENCH)
QBFLAGS := -mlton "~/devel/mlton/builds/20171013.233239-g5513092e3/bin/mlton"
QBFLAGS += -mlton "~/devel/mlton/mlton.git/build/bin/mlton -cse-canon {ascHash,descHash,ascIndex,descIndex}"

.PHONY: qtest
qtest: $(NAME)
	cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && $(MAKE) clean
