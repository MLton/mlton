mlton = mlton

.PHONY: all
all: import export

export: export.sml ffi-export.c
	$(mlton) export.sml ffi-export.c

import: import.sml ffi-import.o
	$(mlton) import.sml ffi-import.o

ffi-import.o:
	$(mlton) -stop o ffi-import.c

clean:
	../../../bin/clean
