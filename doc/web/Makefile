UG = ../user-guide

all:
	cd $(UG) && $(MAKE)
	cp -p ../CHANGES .
	gzip -c $(UG)/main.ps >user-guide.ps.gz
	rm -f HTML/*
	mkdir -p HTML
	cd $(UG)/main &&
		cp -p main.css main.html node*.html *.png ../web/HTML
	cvs co -d benchmarks mlton/src/benchmark/tests
	cvs -Q release benchmarks

.PHONY: clean
clean:
	rm -rf benchmarks CHANGES HTML user-guide.ps.gz
	find . -type f | egrep '.(*~|old|TAGS)$$' | xargs rm -f
