all: benchmarks CHANGES HTML

benchmarks:
	mkdir benchmarks
	cp -pR ../../benchmark/tests/*.sml benchmarks

CHANGES: ../CHANGES
	cp -p ../CHANGES .

HTML:
	cd ../user-guide && make
	gzip -c ../user-guide/main.ps >user-guide.ps.gz
	(cd ../user-guide/main && tar.write .) | \
		(mkdir HTML && cd HTML && tar.read)

.PHONY: clean
clean:
	../../bin/clean
