all:

mlton.ps: mlton.1
	man -t ./mlton.1 >mlton.ps

.PHONY: clean
clean:
	for f in `cat .cvsignore`; do rm -f $$f; done
	rm -f *~
