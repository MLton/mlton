SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
HOST = self
FLAGS = @MLton $(RUNTIME_ARGS) gc-summary -- -host $(HOST)
NAME = mlton
AOUT = mlton-compile
PATH = $(BIN):$(shell echo $$PATH)
# If we're compiling MLton with an older version of itself, use the stubs for
# the MLton structure.  If we're compiling it with itself, don't.
ifeq (self, $(shell if [ -x $(BIN)/mlton ]; then echo self; fi))
  CM = $(NAME).cm
else
ifeq (1997, $(shell $(SRC)/bin/mlton-basis-version))
  CM = $(NAME)-stubs-1997.cm
else
  CM = $(NAME)-stubs.cm
endif
endif

ifeq (old,$(shell PATH=$(BIN):$$PATH; $(MLTON) -v >/dev/null 2>&1 && echo old))
  EXTRA_FLAGS = -v -o $(AOUT)
else
  EXTRA_FLAGS = -verbose 1 -output $(AOUT)
endif

.PHONY: all
all: $(AOUT)

$(AOUT): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling mlton (takes a while)'
	$(MLTON) $(FLAGS) $(EXTRA_FLAGS) $(CM)
	size $(AOUT)

.PHONY:	$(NAME)_cm
$(NAME)_cm: $(NAME)-stubs_cm
	grep -v mlton-stubs $(NAME)-stubs.cm >$(NAME).cm

.PHONY: $(NAME)-stubs-1997_cm
$(NAME)-stubs-1997_cm: $(NAME)-stubs_cm
	(								\
		echo 'Group is' &&					\
		echo '../lib/mlton-stubs/1997-to-2002.sml' &&		\
		grep -v Group $(NAME)-stubs.cm;				\
	) >$(NAME)-stubs-1997.cm

# This makes a version of MLton that can be compiled in the standard basis
# library.  I.E. it doesn't require a MLton structure.
.PHONY:	$(NAME)-stubs_cm
$(NAME)-stubs_cm: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'basis-stubs' | 		\
			grep -v 'mlton-stubs-in-smlnj' |		\
			grep mlyacc &&					\
		cmcat sources.cm | grep -v 'basis-stubs' | 		\
			grep -v 'mlton-stubs-in-smlnj' |		\
			grep -v mlyacc &&				\
		echo 'call-main.sml';					\
	) >$(NAME)-stubs.cm

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
		echo 'val _ = Main.exportNJ ("$(SRC)/basis-library", "$(LIB)/mlton")';	\
		echo 'end';							\
	) >$(NAME).whole.sml
	time $(SML) <$(NAME).whole.sml

mlton.sml: $(NAME).cm $(shell $(MLTON) -stop f mlton.cm)
	rm -f mlton.sml && mlton -stop sml mlton.cm && chmod -w mlton.sml

front-end/ml.lex.sml: front-end/ml.lex
	rm -f front-end/ml.lex.sml
	mllex front-end/ml.lex
	chmod -w front-end/ml.lex.sml

front-end/ml.grm.sig front-end/ml.grm.sml: front-end/ml.grm
	rm -f front-end/ml.grm.*
	mlyacc front-end/ml.grm
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
		echo 'Main.exportNJ ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
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
		echo 'Main.exportNJ ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | time $(SML)

.PHONY: nj-mlton-110.9.1
nj-mlton-110.9.1: front-end/ml.lex.sml front-end/ml.grm.sig front-end/ml.grm.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo 'CM.verbose (SOME false);';				\
		echo 'CM.make'\''{group = "sources.cm", force_relink = true};';	\
		echo 'Main.exportNJ ("$(SRC)/basis-library", "$(LIB)/mlton");'	\
	) | $(SML)
