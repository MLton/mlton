PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton -default-ann 'allowExport true, allowImport true'

.PHONY: all
all: import export iimport
	./import
	./export
	./iimport

export: export.sml ffi-export.c
	$(mlton) -export-header export.h -stop tc export.sml
	gcc -c ffi-export.c
	$(mlton) export.sml ffi-export.o

import: import.sml ffi-import.o
	$(mlton) import.sml ffi-import.o

ffi-import.o:
	$(mlton) -stop o ffi-import.c

iimport: iimport.sml
	$(mlton) iimport.sml

clean:
	../../../bin/clean
