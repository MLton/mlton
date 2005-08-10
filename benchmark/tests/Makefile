## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

all:

.PHONY: clean
clean:
	../../bin/clean
	rm -f *.c *.s
	for f in `ls`; do 			\
		if [ -x $$f -a ! -d $$f ]; then	\
			rm -f $$f;		\
		fi;				\
	done
