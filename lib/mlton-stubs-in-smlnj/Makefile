.PHONY: links
links:
	for f in array cont gc itimer mlton proc-env ptrace random rlimit 	\
			rusage signal socket syslog text-io thread vector	\
			word world; do \
		rm -f $$f.sig && \
		ln ../../basis-library/mlton/$$f.sig .; \
	done
