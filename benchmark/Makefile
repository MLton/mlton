## Copyright (C) 2013,2019,2021-2022 Matthew Fluet.
 # Copyright (C) 2009 Matthew Fluet.
 # Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

NAME := benchmark

all: 	$(NAME)

RUN_MLTON_COMPILE_XARGS := -default-ann 'sequenceNonUnit warn' -default-ann 'warnUnused true'

$(NAME): $(NAME).mlb $(shell $(MLBDEPS) $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS) $(RUN_MLTON_COMPILE_XARGS) -target $(TARGET) $(NAME).mlb

$(eval $(MK_COMMON_GOALS))

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
