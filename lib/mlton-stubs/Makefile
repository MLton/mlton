all: links

.PHONY: links
links:
	for f in `cd ../../basis-library/mlton && ls *.sig`; do 	\
		rm -f $$f && ln ../../basis-library/mlton/$$f .; 	\
	done
	rm -f ffi.sig
	grep -v MLTON_FFI mlton.sig >/tmp/z.sig && mv /tmp/z.sig mlton.sig

.PHONY: clean
clean:
	../../bin/clean
