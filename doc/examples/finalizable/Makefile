PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton -default-ann 'allowImport true'

all:
	$(mlton) finalizable.sml cons.c
	finalizable

.PHONY: clean
clean:
	../../../bin/clean
