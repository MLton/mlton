PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton

all:
	$(mlton) finalizable.sml cons.c
	finalizable

.PHONY: clean
clean:
	../../../bin/clean
