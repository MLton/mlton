TEX_FILES = \
	abstract.tex		\
	basis.tex		\
	bib.bib			\
	bugs.tex		\
	cm.tex			\
	compiling.tex		\
	credits.tex		\
	cross-compiling.tex	\
	cygwin.tex		\
	drawbacks.tex		\
	extensions.tex		\
	features.tex		\
	freebsd.tex		\
	ffi.tex			\
	getting-started.tex	\
	macros.tex		\
	main.tex		\
	man-page.tex		\
	mlb.tex			\
	mlb-formal.tex		\
	nj-deviations.tex	\
	platform.tex		\
	profiling.tex		\
	solaris.tex		\
	type-checking.tex

all:	main.ps main/index.html

main/index.html: $(TEX_FILES)
	mkdir -p main
	hevea -fix -o main/main.html -exec xxdate.exe macros.hva main.tex
	cd main && hacha main.html && rm -f main.html

# -verbosity 0  be quieter (but not quiet enough for my taste)
# -address ''	puts no author address at the bottom of each page

#main/main.html: $(TEX_FILES)
#	latex2html -long_titles 3 -verbosity 1 -address '' -local_icons main.tex
#	cd main && rm -f WARNINGS images.* *.pl
#	cd main && rm -f index.html && ln -s main.html index.html

main.dvi: $(TEX_FILES)
	latex main; bibtex main; latex main; latex main

main.pdf: main.ps
	ps2pdf main.ps

main.ps: main.dvi
	dvips -o main.ps main

.PHONY: clean
clean:
	../../bin/clean

.PHONY: tags
tags:
	etags *.tex
