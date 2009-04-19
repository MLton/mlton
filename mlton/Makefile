## Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
HOST_ARCH := $(shell "$(SRC)/bin/host-arch")
HOST_OS := $(shell "$(SRC)/bin/host-os")
LIB := $(BUILD)/lib
MLTON := mlton
TARGET := self
AOUT := mlton-compile
UP := upgrade-basis.sml
PATH := $(BIN):$(shell echo $$PATH)

FLAGS := @MLton ram-slop 0.7 gc-summary $(RUNTIME_ARGS) --

ifeq (self, $(shell if [ -x "$(BIN)/mlton" ]; then echo self; fi))
  # We're compiling MLton with itself, so don't use any stubs.
  FILE := mlton.mlb
  FLAGS += -default-ann 'sequenceNonUnit warn'
  FLAGS += -default-ann 'warnUnused true'
  # FLAGS += -type-check true -show-types true
else
ifeq (cygwin, $(HOST_OS))
  # The stubs don't work on Cygwin, since they define spawn in terms of
  # fork, and fork doesn't work on Cygwin.  So, make without the stubs.
  FILE := mlton.cm
else
ifeq (mingw, $(HOST_OS))
  # Ditto for MinGW.
  FILE := mlton.cm
else
  # We're compiling MLton with an older version of itself, so use the stubs for
  # the MLton structure.
  FILE := mlton-stubs.cm
endif
endif
endif

ifeq (new,$(shell PATH="$(BIN):$$PATH"; mlton -target self >/dev/null 2>&1 && echo new))
  FLAGS += -target $(TARGET)
else
  FLAGS += -host $(TARGET)
endif
ifeq (new,$(shell PATH="$(BIN):$$PATH"; mlton -verbose 1 >/dev/null 2>&1 && echo new))
  FLAGS += -verbose 2 -output "$(AOUT)"
else
  FLAGS += -v -o "$(AOUT)"
endif
FLAGS += $(COMPILE_ARGS)

SOURCES :=			\
	$(FILE)			\
	$(UP)			\
	front-end/ml.lex.sml	\
	front-end/ml.grm.sig	\
	front-end/ml.grm.sml	\
	front-end/mlb.lex.sml	\
	front-end/mlb.grm.sig	\
	front-end/mlb.grm.sml	\
	$(shell if [ -r $(FILE) ]; then mlton -stop f $(FILE); fi)

.PHONY: all
all: $(AOUT)

front-end/ml.lex.sml: front-end/ml.lex
	$(MAKE) -C front-end ml.lex.sml
front-end/ml.grm.sig front-end/ml.grm.sml: front-end/ml.grm
	$(MAKE) -C front-end ml.grm.sig ml.grm.sml
front-end/mlb.lex.sml: front-end/mlb.lex
	$(MAKE) -C front-end mlb.lex.sml
front-end/mlb.grm.sig front-end/mlb.grm.sml: front-end/mlb.grm
	$(MAKE) -C front-end mlb.grm.sig mlb.grm.sml

control/version.sml: control/version_sml.src
	cat control/version_sml.src |						\
		sed "s/\(.*\)MLTONBUILDDATE\(.*\)/\1`date`\2/" |		\
		sed "s/\(.*\)MLTONBUILDNODE\(.*\)/\1`uname -n`\2/" >		\
		control/version.sml

$(AOUT): $(SOURCES)
	rm -f $(UP)
	$(MAKE) $(UP)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	@echo 'Compiling mlton (takes a while)'
	mlton $(FLAGS) $(FILE)

#! Pass $(PATH) to upgrade-basis because it is run via #!/usr/bin/env
# bash, which resets the path.
$(UP):
	"$(SRC)/bin/upgrade-basis" '$(PATH)' "$(HOST_ARCH)" "$(HOST_OS)" >$(UP)

mlton.sml: $(SOURCES)
	rm -f mlton.sml && mlton -stop sml mlton.cm && chmod -w mlton.sml

.PHONY: def-use
def-use:
	mlton $(FLAGS) -stop tc -prefer-abs-paths true -show-def-use mlton.def-use $(FILE)

.PHONY: clean
clean:
	../bin/clean

#
# The following rebuilds the heap file for the SML/NJ compiled version
# of MLton.
#
SMLNJ := sml
SMLNJ_CM_SERVERS_NUM := 0

.PHONY: smlnj-mlton
smlnj-mlton: $(SOURCES)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'local';							\
		echo 'fun loop 0 = () | loop n = (CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server" ^ (Int.toString n), pathtrans = NONE, pref = 0}; loop (n - 1));'; \
		echo 'in';							\
		echo 'val _ = loop $(SMLNJ_CM_SERVERS_NUM);';			\
		echo 'end;';							\
		echo 'CM.make "mlton-smlnj.cm";';				\
		echo 'SMLofNJ.exportFn("$(LIB)/mlton-smlnj",Main.main);'	\
	) | "$(SMLNJ)"

.PHONY: smlnj-whole
smlnj-whole: $(SOURCES)
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;'; 				\
		echo 'local'; 							\
		cmcat sources.cm | grep 'basis-stubs-for-smlnj' | xargs cat;	\
		cmcat sources.cm | grep 'mlyacc' | xargs cat;			\
		cmcat sources.cm | grep 'mlton-stubs-for-smlnj' | xargs cat;	\
		cmcat sources.cm | grep 'mlton-stubs' | xargs cat;		\
		cmcat sources.cm | 						\
			grep -v 'basis-stubs-for-smlnj' |			\
			grep -v 'mlyacc' |					\
			grep -v 'mlton-stubs-for-smlnj' |			\
			grep -v 'mlton-stubs' | xargs cat;	 		\
		echo 'in';							\
		echo 'SMLofNJ.exportFn("$(LIB)/mlton-smlnj",Main.main);'	\
		echo 'end';							\
	) >mlton.whole.sml
	"$(SMLNJ)" <mlton.whole.sml

#
# The following rebuilds the executable file for the Poly/ML compiled
# version of MLton.
#
POLYML	:= poly

.PHONY: polyml-mlton
polyml-mlton: mlton-polyml.use
	rm -f control/version.sml
	$(MAKE) control/version.sml
	(									\
		echo 'use "mlton-polyml.use";';					\
		echo 'PolyML.export("mlton-polyml", Main.mainWrapped);';	\
	) | "$(POLYML)"
	$(CC) -o $(LIB)/mlton-polyml mlton-polyml.o -lpolymain -lpolyml
	rm -f mlton-polyml.o

mlton-polyml.use: ../lib/stubs/basis-stubs-for-polyml/sources.use ../lib/stubs/mlton-stubs-for-polyml/sources.use $(shell mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(shell mlton -stop f mlton.mlb)
	(									\
		cat ../lib/stubs/basis-stubs-for-polyml/sources.use |		\
			sed 's|use "\(.*\)";|../lib/stubs/basis-stubs-for-polyml/\1|'; \
		mlton -stop f mlton.mlb | grep -v 'mlb$$' | grep 'mlyacc';	\
		cat ../lib/stubs/mlton-stubs-for-polyml/sources.use |		\
			sed 's|use "\(.*\)";|../lib/stubs/mlton-stubs-for-polyml/\1|'; \
		mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb |		\
			grep -v 'mlb$$' | 					\
			grep 'mlton-stubs';					\
		mlton -stop f mlton.mlb |					\
			grep -v 'mlb$$' |					\
			grep -v 'sml/basis' | 					\
			grep -v 'mlton-stubs' |					\
			grep -v 'mlyacc' |					\
			grep -v 'call-main.sml';				\
	) | sed 's|\(.*\)|use "\1";|' > mlton-polyml.use

.PHONY: warn
warn:
	mlton -stop tc -default-ann 'warnUnused true' $(FILE) \
		>/tmp/z.warn 2>&1
