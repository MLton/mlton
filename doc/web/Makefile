UG = ../user-guide

all:
	cp -p ../CHANGES .
	cd $(UG) && $(MAKE)
	gzip -c $(UG)/main.ps >user-guide.ps.gz
	rm -rf HTML
	mkdir -p HTML
	(cd $(UG)/main && tar.write .) | (cd HTML && tar.read)
	rm -rf benchmarks
	cvs -Q co -d benchmarks mlton/benchmark/tests
	cvs -Q release benchmarks
	find benchmarks -type d | grep CVS | xargs rm -rf

.PHONY: clean
clean:
	rm -rf benchmarks CHANGES HTML user-guide.ps.gz
	find . -type f | egrep '.(*~|old|TAGS)$$' | xargs rm -f
