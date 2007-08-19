## Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH := ../../../build/bin:$(shell echo $$PATH)

mlton := mlton -default-ann 'allowFFI true'

all:
	$(mlton) finalizable.sml cons.c
	finalizable

.PHONY: clean
clean:
	../../../bin/clean
