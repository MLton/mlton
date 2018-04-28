## Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

TEX_FILES := \
	bib.bib			\
	mlb-formal.tex

all:	mlb-formal.ps mlb-formal.pdf

mlb-formal.dvi: $(TEX_FILES)
	latex mlb-formal; bibtex mlb-formal; latex mlb-formal; latex mlb-formal

mlb-formal.pdf: $(TEX_FILES)
	latex mlb-formal
	bibtex mlb-formal
	latex mlb-formal
	pdflatex mlb-formal

mlb-formal.ps: mlb-formal.dvi
	dvips -o mlb-formal.ps mlb-formal

.PHONY: clean
clean:
	../../bin/clean

.PHONY: tags
tags:
	etags *.tex
