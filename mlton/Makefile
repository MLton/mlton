ROOT = ../..
BIN = $(ROOT)/bin
LIB = $(ROOT)/lib
SRC = $(ROOT)/src
MLTON = mlton
NAME = mlton
AOUT = mlton-compile
FLAGS = @MLton -- -v2 -native-live-transfer false -o $(AOUT)
PATH = $(BIN):$(shell echo $$PATH)

all: $(AOUT)

$(AOUT): $(shell $(MLTON) -stop f mlton.cm)
	@echo 'Compiling mlton (takes a while)'
	time $(MLTON) $(FLAGS) mlton.cm
	size $(AOUT)
	strip $(AOUT)

$(NAME).cm: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(								\
		echo 'Group is';					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' |	\
			grep mlyacc;					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' |	\
			grep -v mlyacc;					\
		echo 'call-main.sml';					\
	) >$(NAME).cm

mlton.sml: $(NAME).cm $(shell $(MLTON) -stop f mlton.cm)
	mlton -stop sml mlton.cm

front-end/ml.lex.sml: front-end/ml.lex
	mllex front-end/ml.lex

front-end/ml.grm.sig front-end/ml.grm.sml: front-end/ml.grm
	mlyacc front-end/ml.grm

.PHONY: clean
clean:
	find . -type f | egrep '.(*~|old|TAGS)$$' | xargs rm -f
	find . -type d | grep 'CM$$' | xargs rm -rf
	rm -f mlton.sml mlton-compile mlmon.out
	rm -f front-end/ml.lex.sml front-end/ml.grm.*

#
# The following rebuilds the heap file for the SML/NJ compiled version
# of MLton. 
# It requires that you have SML/NJ with the Compilation
# Manager (CM) installed.  You may need to replace the following with
# 'sml-cm'.
#
SML	= sml

.PHONY: nj-mlton
nj-mlton: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo 'CM.verbose(SOME false);';					\
		echo 'CM.make'\''{group = "sources.cm", force_relink = true};';	\
		echo 'Main.exportNJ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | $(SML)
