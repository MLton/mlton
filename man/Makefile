all:

.PHONY: ps
ps: mllex.ps mlprof.ps mlton.ps

%.ps: %.1
	man -t ./$< >$@

.PHONY: clean
clean:
	../bin/clean
