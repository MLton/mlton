SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
TARGET = self
FLAGS = -target $(TARGET) -default-ann 'sequenceUnit true, warnUnused true'
NAME = benchmark
PATH = $(BIN):$(shell echo $$PATH)

all: 	$(NAME)

$(NAME): $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(FLAGS) $(NAME).cm
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY: clean
clean:
	../bin/clean

.PHONY: install
install: $(NAME)
	if [ `whoami` != 'root' ]; then echo >&2 'You should be root'; exit 1; fi
	cp -p $(NAME) /usr/bin/

.PHONY: tags
tags:
	etags *.fun *.sig *.sml

BENCH = barnes-hut boyer checksum count-graphs DLXSimulator fft fib flat-array hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc model-elimination mpuz nucleic output1 peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern
FPBENCH = barnes-hut fft hamlet mandelbrot matrix-multiply nucleic ray raytrace simple tensor tsp tyan vliw zern

BFLAGS = -mlton "/usr/bin/mlton" -mlton "mlton -optimize-ssa {false,true}"
BFLAGS = -html -mlton "/usr/bin/mlton" -mlkit -mosml -poly -smlnj
BFLAGS = -mlton "$(BIN)/mlton"

.PHONY: test
test: $(NAME)
	cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH = $(BENCH)
QBFLAGS = -mlton "mlton{-stable,.cvs.HEAD -detect-overflow {false,true}}"

.PHONY: qtest
qtest: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && $(MAKE) clean
