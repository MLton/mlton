PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton -default-ann 'allowExport true' -default-ann 'allowImport true'

.PHONY: all
all: import import2 export iimport
	./import
	./import2
	./export
	./iimport

export: export.sml ffi-export.c
	$(mlton) -export-header export.h -stop tc export.sml
	$(mlton) -stop o ffi-export.c
	$(mlton) export.sml ffi-export.o

import: import.sml ffi-import.o
	$(mlton) import.sml ffi-import.o

import2: import2.sml ffi-import.o
	$(mlton) import2.sml ffi-import.o

ffi-import.o:
	$(mlton) -stop o ffi-import.c

iimport: iimport.sml
	$(mlton) 				\
		-target-link-opt linux -ldl 	\
		-target-link-opt solaris -ldl	\
		iimport.sml

clean:
	../../../bin/clean
