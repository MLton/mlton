.PHONY: links
links:
	for f in `cd ../../basis-library/mlton && ls *.sig`; do 	\
		rm -f $$f && ln ../../basis-library/mlton/$$f .; 	\
	done

.PHONY: clean
clean:
	../../bin/clean
