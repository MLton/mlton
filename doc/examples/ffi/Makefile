## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH  := ../../../build/bin:$(shell echo $$PATH)
mlton := mlton -default-ann 'allowFFI true'

.PHONY: all all-win32
all-win32: import import2 export test_quot
	./import
	./import2
	./export
	./test_quot
all:	all-win32 iimport
	./iimport

export: export.sml ffi-export.o
import: import.sml ffi-import.o
import2: import2.sml ffi-import.o
test_quot : test_quot.sml c_quot.o

ffi-import.o: export.h
ffi-export.o: export.h
c_quot.o : test_quot.h

iimport: iimport.sml
	$(mlton) 				\
		-target-link-opt linux -ldl 	\
		-target-link-opt solaris -ldl	\
		iimport.sml

%.o:	%.c
	gcc -Wall -c -o $@ $<
%.h:	%.sml
	$(mlton) -export-header $@ -stop tc $<
%:
	$(mlton) -output $@ $^

clean:
	../../../bin/clean
