## Copyright (C) 2013,2018,2022 Matthew Fluet
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

PDFLATEX := pdflatex

mlyacc.pdf: mlyacc.tex
	$(PDFLATEX) mlyacc.tex
	$(PDFLATEX) mlyacc.tex
	$(PDFLATEX) mlyacc.tex

$(eval $(MK_COMMON_GOALS))
