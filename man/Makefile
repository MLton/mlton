## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

all:

.PHONY: ps
ps: mllex.ps mlprof.ps mlton.ps

%.pdf: %.ps
	ps2pdf $<

%.ps: %.1
	man -t ./$< >$@

.PHONY: clean
clean:
	../bin/clean
