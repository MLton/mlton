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
