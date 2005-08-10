## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

all: mlyacc.ps

html/index.html: $(TEX_FILES)
	mkdir -p html
	hevea -fix -o html/mlyacc.html -exec xxdate.exe macros.hva mlyacc.tex
	cd html && hacha mlyacc.html && rm -f mlyacc.html

mlyacc.dvi: mlyacc.tex
	latex mlyacc.tex
	latex mlyacc.tex

mlyacc.pdf: mlyacc.tex
	latex mlyacc.tex
	pdflatex mlyacc.tex

mlyacc.ps: mlyacc.dvi
	dvips -o mlyacc.ps mlyacc.dvi
