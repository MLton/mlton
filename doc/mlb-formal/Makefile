## Copyright (C) 2022 Matthew Fluet
 # Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

PDFLATEX := pdflatex
BIBTEX := bibtex

TEX_FILES := 		\
	bib.bib		\
	mlb-formal.tex

all: mlb-formal.pdf

mlb-formal.pdf: $(TEX_FILES)
	$(PDFLATEX) mlb-formal.tex
	$(BIBTEX) mlb-formal
	$(PDFLATEX) mlb-formal.tex
	$(PDFLATEX) mlb-formal.tex

$(eval $(MK_COMMON_GOALS))
