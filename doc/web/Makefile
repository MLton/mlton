all: benchmarks CHANGES HTML

benchmarks:
	mkdir benchmarks
	cp -pR ../../benchmark/tests/*.sml benchmarks/
	cp -pR ../../benchmark/tests/DATA benchmarks/

CHANGES: ../CHANGES
	cp -p ../CHANGES ./

user-guide:
	cd ../user-guide && make
	gzip -c ../user-guide/main.ps >user-guide.ps.gz
	(cd ../user-guide/main && tar.write .) | \
		(mkdir user-guide && cd user-guide && tar.read)

.PHONY: clean
clean:
	../../bin/clean
