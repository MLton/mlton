## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
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
