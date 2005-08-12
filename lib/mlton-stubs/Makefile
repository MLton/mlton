## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

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
