NAME = benchmark
MLTON = mlton
HOST = self
FLAGS = -host $(HOST)
PATH = $(shell cd ../build/bin && echo `pwd`:$$PATH)

all: 	$(NAME)

$(NAME): $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	strip $(NAME)
	size $(NAME)

.PHONY: $(NAME)_cm
$(NAME)_cm:
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME).cm

.PHONY: clean
clean:
	../bin/clean

.PHONY: install
install: $(NAME)
	if [ `whoami` != 'root' ]; then echo >&2 'You should be root'; exit 1; fi
	cp -p $(NAME) /usr/local/bin

.PHONY: tags
tags:
	etags *.fun *.sig *.sml

BENCH = barnes-hut checksum count-graphs DLXSimulator fft fib hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc mpuz nucleic peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern
FPBENCH = barnes-hut fft hamlet mandelbrot matrix-multiply nucleic ray raytrace simple tensor tsp tyan vliw zern

BFLAGS = -mlton "/usr/local/bin/mlton" -mlton "mlton -optimize-ssa {false,true}"
BFLAGS = -mlton "mlton"
BFLAGS = -html -mlton "mlton" -mlkit -mosml -poly -smlnj

.PHONY: test
test: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH = $(BENCH)

QBFLAGS = -mlton "mlton-stable" -mlton "mlton" -mlton "mlton -native-reserve-esp true" 

.PHONY: qtest
qtest: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && make clean
