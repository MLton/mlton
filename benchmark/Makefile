NAME = benchmark
MLTON = mlton-stable
FLAGS =
PATH = $(shell cd ../../bin && pwd):$(shell echo $$PATH)

all:	$(NAME)

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

BENCH = barnes-hut checksum count-graphs DLXSimulator fft fib hamlet knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc mpuz nucleic peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern

BFLAGS = -mlton "mlton" -mlkit -mosml -smlnj
BFLAGS = -mlton "mlton"

.PHONY: test
test: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH = $(BENCH)

QBFLAGS = -mlton "mlton -drop-pass removeUnused1CPS -drop-pass leafInlineCPS -drop-pass raiseToJump1CPS -drop-pass contify1CPS -drop-pass localFlatten1CPS -drop-pass uselessCPS -drop-pass removeUnused2CPS -drop-pass unusedArgs1CPS -drop-pass simplifyTypesCPS -drop-pass polyEqualCPS -drop-pass contify2CPS -drop-pass inlineCPS -drop-pass localFlatten2CPS -drop-pass removeUnused3CPS -drop-pass raiseToJump2CPS -drop-pass contify3CPS -drop-pass unusedArgs2CPS -drop-pass introduceLoopsCPS -drop-pass loopInvariantCPS -drop-pass flattenCPS -drop-pass localFlatten3CPS -drop-pass commonSubexpCPS -drop-pass commonBlockCPS -drop-pass redundantTestsCPS -drop-pass redundantCPS -drop-pass unusedArgs3CPS -drop-pass removeUnused4CPS"

.PHONY: qtest
qtest: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && make clean
