## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH := ../../../build/bin:$(shell echo $$PATH)

mlton := mlton -default-ann 'allowFFI true'

.PHONY: all
all: import import2 export iimport test_quot
	./import
	./import2
	./export
	./iimport
	./test_quot

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

c_quot.o : c_quot.c test_quot.h
	$(mlton) -stop o c_quot.c

test_quot.h : test_quot.sml
	$(mlton) -export-header test_quot.h -stop tc test_quot.sml

test_quot : test_quot.sml c_quot.o
	$(mlton) test_quot.sml c_quot.o

clean:
	../../../bin/clean
