NAME = benchmark
MLTON = mlton
FLAGS =
PATH = $(shell cd ../../bin && pwd):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	strip $(NAME)
	size $(NAME)

$(NAME).cm:
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME).cm

OLD = old
.PHONY: backup
backup:	clean
	if [ ! -d $(OLD) ]; then mkdir $(OLD); fi
	tar.write -x $(OLD) -x CM . >$(OLD)/`date +%Y-%m-%d`.tar

.PHONY: clean
clean:
	find . -type f | grep '.*~$$' | xargs rm -f
	find . -type d | grep 'CM$$' | xargs rm -rf
	rm -f junk $(NAME) $(NAME).sml
	cd tests && make clean

.PHONY: install
install: $(NAME)
	if [ `whoami` != 'root' ]; then echo >&2 'You should be root'; exit 1; fi
	cp -p $(NAME) /usr/local/bin

.PHONY: tags
tags:
	etags *.fun *.sig *.sml

BENCH = barnes-hut checksum count-graphs DLXSimulator fft fib hamlet knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc mpuz nucleic peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern

BFLAGS = -mlton "mlton" -mlkit -mosml -smlnj

.PHONY: test
test: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(BFLAGS) $(BENCH)

QBENCH = $(BENCH)

QBFLAGS = -mlton "mlton-stable -native-live-stack {true}" -mlton "mlton -native-live-stack {true} -native-live-transfer {0,4,8}"

.PHONY: qtest
qtest: $(NAME)
	export PATH=$(PATH):$$PATH && cd tests && ../benchmark $(QBFLAGS) $(QBENCH) && make clean
