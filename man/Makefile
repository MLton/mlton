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
