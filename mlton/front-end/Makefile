.PHONY: all
all: ml.lex.sml ml.grm.sig ml.grm.sml

.PHONY: clean
clean:
	../../bin/clean

ml.lex.sml: ml.lex
	rm -f ml.lex.sml
	mllex ml.lex
	chmod -w ml.lex.sml

ml.grm.sig ml.grm.sml: ml.grm
	rm -f ml.grm.*
	mlyacc ml.grm
	chmod -w ml.grm.*
