## Copyright (C) 2013 Matthew Fluet
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

html/index.html: $(TEX_FILES)
	mkdir -p html
	hevea -fix -o html/mlyacc.html -exec xxdate.exe macros.hva mlyacc.tex
	cd html && hacha mlyacc.html && rm -f mlyacc.html

PDFLATEX := pdflatex

mlyacc.pdf: mlyacc.tex
	$(PDFLATEX) mlyacc.tex
	$(PDFLATEX) mlyacc.tex

.PHONY: clean
clean:
	../../bin/clean
