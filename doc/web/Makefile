UG = ../user-guide

all:
	cd $(UG) && $(MAKE)
	cp -p ../CHANGES .
	gzip -c $(UG)/main.ps >user-guide.ps.gz
	rm -f HTML/*
	mkdir -p HTML
	cd $(UG)/main && \
		cp -p main.css main.html node*.html *.png ../../web/HTML
	cvs -Q co -d benchmarks mlton/benchmark/tests
	cvs -Q release benchmarks
	find benchmarks -type d | grep CVS | xargs rm -rf

.PHONY: clean
clean:
	rm -rf benchmarks CHANGES HTML user-guide.ps.gz
	find . -type f | egrep '.(*~|old|TAGS)$$' | xargs rm -f
