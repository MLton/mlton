TEX_FILES = \
	abstract.tex		\
	basis.tex		\
	bugs.tex		\
	cm.tex			\
	compiling.tex		\
	credits.tex		\
	drawbacks.tex		\
	extensions.tex		\
	features.tex		\
	ffi.tex			\
	getting-started.tex	\
	macros.tex		\
	main.tex		\
	man-page.tex		\
	nj-deviations.tex	\
	performance.tex		\
	profiling.tex		\

WEB = ../web

all:	main.ps main/main.html

main/main.html: $(TEX_FILES)
	latex2html -address '' -local_icons main.tex

main.dvi: $(TEX_FILES)
	latex main; bibtex main; latex main; latex main
	
main.ps: main.dvi
	dvips -o main.ps main

.PHONY: clean
clean:
	rm -rf main TAGS core main.ps.gz main.aux main.bbl main.blg main.dvi main.log main.ps main.toc *~

.PHONY: tags
tags:
	etags *.tex
