all:

%: %.sml
	mlton $<

.PHONY:
clean:
	for d in ffi profiling save-world;		\
	do						\
		cd $$d && $(MAKE) clean && cd ..;	\
	done
	rm -f *~
	for f in `ls`; do 				\
		if [ -x $$f -a ! -d $$f ]; then 	\
			rm -f $$f; 			\
		fi; 					\
	done
