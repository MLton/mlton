TEX_FILES = \
	abstract.tex		\
	basis-library.tex	\
	main.tex		\
	macros.tex		\
	mlton.tex		\
	runtime.tex		\
	sources.tex		\

FIG_FILES = \
	structure.ps		\

all:	main.ps

# -verbosity 0  be quieter (but not quiet enough for my taste)
# -address ''	puts no author address at the bottom of each page

main/main.html: $(TEX_FILES)
	latex2html -verbosity 1 -address '' -local_icons main.tex
	cd main && rm -f WARNINGS images.* *.pl
	cd main && rm -f index.html && ln -s main.html index.html

main.dvi: $(TEX_FILES) $(FIG_FILES)
	latex main; bibtex main; latex main; latex main
	
main.ps: main.dvi
	dvips -o main.ps main

%.ps: %.fig
	fig2dev -Leps $< $@

.PHONY: clean
clean:
	../../bin/clean

.PHONY: tags
tags:
	etags *.tex
