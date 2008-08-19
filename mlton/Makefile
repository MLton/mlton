## Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
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
	front-end/ml.lex.sml 	\
	front-end/ml.grm.sig 	\
	front-end/ml.grm.sml	\
	front-end/mlb.lex.sml 	\
	front-end/mlb.grm.sig 	\
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

$(AOUT): $(SOURCES)
	rm -f $(UP)
	$(MAKE) $(UP)
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
# It requires that you have SML/NJ with the Compilation
# Manager (CM) installed.  You may need to replace the following with
# 'sml-cm'.
#
SML	:= sml

.PHONY: nj-mlton
nj-mlton: $(SOURCES)
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'CM.make "sources.cm";';					\
		echo 'Main.exportNJ ("$(LIB)/mlton");'	\
	) | "$(SML)"

.PHONY: nj-mlton-dual
nj-mlton-dual: $(SOURCES)
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server1", pathtrans = NONE, pref = 0};';\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server2", pathtrans = NONE, pref = 0};';\
		echo 'CM.make "sources.cm";';					\
		echo 'Main.exportNJ ("$(LIB)/mlton");'	\
	) | "$(SML)"

.PHONY: nj-mlton-quad
nj-mlton-quad: $(SOURCES)
	(									\
		echo 'SMLofNJ.Internals.GC.messages false;';			\
		echo '#set CM.Control.verbose false;';				\
		echo '#set CM.Control.warn_obsolete false;';			\
		echo 'Control.polyEqWarn := false;';				\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server1", pathtrans = NONE, pref = 0};';\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server2", pathtrans = NONE, pref = 0};';\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server3", pathtrans = NONE, pref = 0};';\
		echo 'val _ = CM.Server.start {cmd = (CommandLine.name (), ["@CMslave"]), name = "server4", pathtrans = NONE, pref = 0};';\
		echo 'CM.make "sources.cm";';					\
		echo 'Main.exportNJ ("$(LIB)/mlton");'	\
	) | "$(SML)"

.PHONY: nj-whole
nj-whole: $(SOURCES)
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
	) >mlton.whole.sml
	"$(SML)" <mlton.whole.sml

.PHONY: warn
warn:
	mlton -stop tc -default-ann 'warnUnused true' $(FILE) \
		>/tmp/z.warn 2>&1
