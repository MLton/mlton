all: 

.PHONY: clean
clean:
	../bin/clean
	rm -f *.[csS]
	for f in `ls`; do 			\
		if [ -x $$f -a ! -d $$f ]; then	\
			rm -f $$f;		\
		fi;				\
	done


.PHONY: abnormal
abnormal:
	ls -1 | egrep -v '\.ok$$|\.sml$$'


