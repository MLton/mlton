ROOT = $(shell cd ../.. && pwd)

all: 

.PHONY: clean
clean:
	$(ROOT)/src/bin/clean
	rm -rf *.dat outFuhMishra* textio.tmp PM hardlinkA hardlinkB \
		testbadl testcycl testlink hardlinkA hardlinkB
	for d in fail modules nonterminate; do \
 		cd $$d && $(ROOT)/src/bin/clean && cd ..; \
	done
	for f in `ls`; do 				\
		if [ -x $$f -a ! -d $$f ]; then 	\
			rm -f $$f; 			\
		fi; 					\
	done


.PHONY: abnormal
abnormal:
	ls -1 | egrep -v '\.ok$$|\.sml$$'


