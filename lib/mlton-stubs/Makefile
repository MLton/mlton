all: links

.PHONY: links
links:
	for f in `cd ../../basis-library/mlton && ls *.sig`; do 	\
		if [ "$$f" != 'ffi.sig' ]; then				\
			rm -f $$f && 					\
				ln ../../basis-library/mlton/$$f .; 	\
		fi							\
	done

.PHONY: clean
clean:
	../../bin/clean
