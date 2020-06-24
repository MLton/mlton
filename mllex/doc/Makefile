## Copyright (C) 2020 Matthew Fluet
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

PDFLATEX := pdflatex

lexgen.pdf: lexgen.tex
	$(PDFLATEX) lexgen.tex
	$(PDFLATEX) lexgen.tex
	$(PDFLATEX) lexgen.tex

.PHONY: clean
clean:
	../../bin/clean
