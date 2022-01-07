## Copyright (C) 2020,2022 Matthew Fluet
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

PDFLATEX := pdflatex

ml-lex.pdf: ml-lex.tex
	$(PDFLATEX) ml-lex.tex
	$(PDFLATEX) ml-lex.tex
	$(PDFLATEX) ml-lex.tex

$(eval $(MK_COMMON_GOALS))
