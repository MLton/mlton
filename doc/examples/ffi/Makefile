mlton = mlton

.PHONY: all
all: import export

export: export.sml ffi-export.c
	$(mlton) -export-header true >export.h export.sml
	gcc -c -I/usr/lib/mlton/self/include ffi-export.c
	$(mlton) -debug true -native false export.sml ffi-export.o

import: import.sml ffi-import.o
	$(mlton) import.sml ffi-import.o

ffi-import.o:
	$(mlton) -stop o ffi-import.c

clean:
	../../../bin/clean
