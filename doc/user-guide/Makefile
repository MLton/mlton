TEX_FILES = \
	abstract.tex		\
	basis.tex		\
	bugs.tex		\
	cm.tex			\
	compiling.tex		\
	credits.tex		\
	cross-compiling.tex	\
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

all:	main.ps main/main.html

# -verbosity 0  be quieter (but not quiet enough for my taste)
# -address ''	puts no author address at the bottom of each page

main/main.html: $(TEX_FILES)
	latex2html -verbosity 1 -address '' -local_icons main.tex
	cd main && rm -f WARNINGS images.* *.pl
	cd main && rm -f index.html && ln -s main.html index.html

main.dvi: $(TEX_FILES)
	latex main; bibtex main; latex main; latex main
	
main.ps: main.dvi
	dvips -o main.ps main

.PHONY: clean
clean:
	../../bin/clean

.PHONY: tags
tags:
	etags *.tex
