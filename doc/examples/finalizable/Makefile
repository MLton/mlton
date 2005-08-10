## Copyright (C) 2003-2004 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton -default-ann 'allowFFI true'

all:
	$(mlton) finalizable.sml cons.c
	finalizable

.PHONY: clean
clean:
	../../../bin/clean
