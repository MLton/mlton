all:

%: %.sml
	mlton $<

get:
	for f in callcc command-line hello-world same-fringe signals size taut thread1 thread2 thread-switch timeout; do \
 		cp ../../regression/$$f.sml .; \
	done


.PHONY:
clean:
	../../bin/clean
	rm -f *.sml
