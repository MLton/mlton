ROOT = ../..
BIN = $(ROOT)/bin
LIB = $(ROOT)/lib
SRC = $(ROOT)/src
MLTON = mlton
NAME = mlton
AOUT = mlton-compile
FLAGS = @MLton gc-summary max-heap 475m -- -v3 -o $(AOUT)
PATH = $(BIN):$(shell echo $$PATH)

all: $(AOUT)

$(AOUT): $(NAME).cm $(shell $(MLTON) -stop f mlton.cm)
	@echo 'Compiling mlton (takes a while)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	size $(AOUT)
	strip $(AOUT)

.PHONY:	$(NAME)_cm
$(NAME)_cm: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(								\
		echo 'Group is'&&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' |	\
			grep mlyacc &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' |	\
			grep -v mlyacc &&				\
		echo 'call-main.sml';					\
	) >$(NAME).cm

.PHONY: nj-whole
nj-whole: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;'; 				\
		echo 'local'; 							\
		cmcat sources.cm | grep 'mlton-stubs-in-smlnj' | xargs cat;	\
		cmcat sources.cm | grep 'mlyacc' | xargs cat;			\
		cmcat sources.cm | grep -v 'mlyacc' |				\
			grep -v 'mlton-stubs-in-smlnj' | xargs cat; 		\
		echo 'in';							\
		echo 'val _ = Main.exportNJ("$(SRC)/basis-library", "$(LIB)/mlton")';	\
		echo 'end';							\
	) >$(NAME).whole.sml
	time $(SML) <$(NAME).whole.sml

mlton.sml: $(NAME).cm $(shell $(MLTON) -stop f mlton.cm)
	rm -f mlton.sml && mlton -stop sml mlton.cm && chmod -w mlton.sml

front-end/ml.lex.sml: front-end/ml.lex
	rm -f front-end/ml.lex.sml && \
		mllex front-end/ml.lex && \
		chmod -w front-end/ml.lex.sml

front-end/ml.grm.sig front-end/ml.grm.sml: front-end/ml.grm
	rm -f front-end/ml.grm.* && \
		mlyacc front-end/ml.grm && \
		chmod -w front-end/ml.grm.*

.PHONY: clean
clean:
	../bin/clean

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
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'CM.make "sources.cm";';					\
		echo 'Main.exportNJ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | time $(SML)

.PHONY: nj-mlton-dual
nj-mlton-dual: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server1", pathtrans = NONE, pref = 0};';\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server2", pathtrans = NONE, pref = 0};';\
		echo 'CM.make "sources.cm";';					\
		echo 'Main.exportNJ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | time $(SML)

.PHONY: nj-mlton-110.9.1
nj-mlton-110.9.1: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo 'CM.verbose(SOME false);';					\
		echo 'CM.make'\''{group = "sources.cm", force_relink = true};';	\
		echo 'Main.exportNJ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | $(SML)
