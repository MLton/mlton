.PHONY: all
all: CHANGES benchmarks user-guide

benchmarks:
	mkdir benchmarks
	cp -pR ../../benchmark/tests/*.sml benchmarks/
	cp -pR ../../benchmark/tests/DATA benchmarks/

CHANGES: ../CHANGES
	cp -p ../CHANGES ./

.PHONY: user-guide
user-guide:
	rm -rf user-guide
	$(MAKE) -C ../user-guide
	gzip -c ../user-guide/main.ps >user-guide.ps.gz
	(cd ../user-guide/main && tar.write .) | \
		(mkdir user-guide && cd user-guide && tar.read)

.PHONY: clean
clean:
	../../bin/clean
