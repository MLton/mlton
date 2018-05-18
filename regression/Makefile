## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

all: 

.PHONY: clean
clean:
	../bin/clean
	rm -f *.[csS]
	for f in *; do 			\
		if [ -x "$$f" -a ! -d "$$f" ]; then	\
			rm -f "$$f";		\
		fi;				\
	done


.PHONY: abnormal
abnormal:
	ls -1 | egrep -v '\.ok$$|\.sml$$'
