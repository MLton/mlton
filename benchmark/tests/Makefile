all:

.PHONY: clean
clean:
	../../bin/clean
	rm -f *.mlton.sml *.batch.sml *.uo *.ui *.x86-linux	\
		a.out run z z.sml TEST
	rm -rf chess.ppm
	rm -rf PM
	for f in `ls`; do 				\
		if [ -x $$f -a ! -d $$f ]; then 	\
			rm -f $$f; 			\
		fi; 					\
	done

