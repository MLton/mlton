## Copyright (C) 2010,2013 Matthew Fluet.
 # Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
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
TARGET := self
AOUT := mlton-compile
PATH := $(BIN):$(shell echo $$PATH)

FLAGS := @MLton ram-slop 0.7 gc-summary $(RUNTIME_ARGS) --

ifeq (self, $(shell if [ -x "$(BIN)/mlton" ]; then echo self; fi))
  # We're compiling MLton with itself, so don't use any stubs.
  FILE := mlton.mlb
  FLAGS += -default-ann 'sequenceNonUnit warn'
  FLAGS += -default-ann 'warnUnused true'
  # FLAGS += -type-check true
else
# We're compiling MLton with an older version of itself. 
# Use "-align 8" for amd64 to avoid GMP/mul_2exp segfault with a
# mis-aligned limb when compiling with <= 20070826.
ifeq (amd64,$(findstring $(HOST_ARCH), amd64))
  FLAGS += -align 8
endif
ifneq (,$(findstring $(HOST_OS), cygwin mingw))
  # The stubs don't work on Cygwin or MinGW, since they define spawn
  # in terms of fork, and fork doesn't work on Cygwin or MinGW.  So,
  # make without the stubs.
  FILE := mlton.mlb
else
  # We're compiling MLton with an older version of itself, so use the stubs for
  # the MLton structure.
  FILE := mlton-stubs.mlb
endif
endif

FLAGS += -target $(TARGET)
FLAGS += -verbose 2 -output "$(AOUT)"
FLAGS += $(COMPILE_ARGS)

FRONT_END_SOURCES :=		\
	front-end/ml.lex.sml	\
	front-end/ml.grm.sig	\
	front-end/ml.grm.sml	\
	front-end/mlb.lex.sml	\
	front-end/mlb.grm.sig	\
	front-end/mlb.grm.sml

SOURCES :=			\
	$(FILE)			\
	upgrade-basis.sml	\
	$(FRONT_END_SOURCES)	\
	$(filter-out control/version.sml,$(shell if [ -r $(FILE) ]; then mlton -stop f $(FILE) | grep -v " "; fi))

.PHONY: all
all: $(AOUT)

control/version.sml: control/version_sml.src
	cat control/version_sml.src |				\
		sed "s/MLTONVERSION/$$(git log -n1 --pretty="g%h$$(if [ $$(git status --porcelain 2> /dev/null | wc -l) -ne 0 ]; then echo '-dirty'; fi)" 2> /dev/null || echo "???????")/" |	\
		sed "s/MLTONBUILDDATE/$$(LANG=en_us date)/" |	\
		sed "s/MLTONBUILDNODE/$$(uname -n)/" >		\
		control/version.sml

front-end/%.lex.sml: front-end/%.lex
	$(MAKE) -C front-end $(@F)
front-end/%.grm.sig front-end/%.grm.sml: front-end/%.grm
	$(MAKE) -C front-end $(<F).sig $(<F).sml

# Pass $(PATH) to upgrade-basis because it is run via
# #!/usr/bin/env bash, which resets the path.
upgrade-basis.sml:
	"$(SRC)/bin/upgrade-basis" '$(PATH)' "$(HOST_ARCH)" "$(HOST_OS)" >upgrade-basis.sml

mlton-stubs.mlb: $(shell mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(shell mlton -stop f mlton.mlb)
	(									\
		echo '$$(SML_LIB)/basis/unsafe.mlb';				\
		echo '$$(SML_LIB)/basis/sml-nj.mlb';				\
		echo '$$(SML_LIB)/basis/mlton.mlb';				\
		echo '$$(SML_LIB)/basis/basis.mlb';				\
		echo 'upgrade-basis.sml';					\
		mlton -stop f mlton.mlb | grep -v 'mlb$$' | grep 'mlyacc';	\
		mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb |		\
			grep -v 'mlb$$' | 					\
			grep 'mlton-stubs';					\
		mlton -stop f mlton.mlb |					\
			grep -v 'mlb$$' |					\
			grep -v 'sml/basis' | 					\
			grep -v 'targets' | 					\
			grep -v 'mlyacc';					\
	) > mlton-stubs.mlb

$(AOUT): $(SOURCES)
	rm -f upgrade-basis.sml
	$(MAKE) upgrade-basis.sml
	rm -f control/version.sml
	$(MAKE) control/version.sml
	@echo 'Compiling mlton (takes a while)'
	mlton $(FLAGS) $(FILE)

.PHONY: def-use
def-use: mlton.def-use

mlton.def-use: $(SOURCES)
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
smlnj-mlton: $(FRONT_END_SOURCES)
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
		echo 'if (CM.make "mlton-smlnj.cm") handle _ => false';		\
		echo '   then ()';						\
		echo '   else OS.Process.exit OS.Process.failure;'; 		\
		echo 'SMLofNJ.exportFn("mlton-smlnj",Main.main);'		\
	) | "$(SMLNJ)"

#
# The following rebuilds the executable file for the Poly/ML compiled
# version of MLton.
#
POLYML	:= poly

.PHONY: polyml-mlton
polyml-mlton: mlton-polyml.use $(FRONT_END_SOURCES)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	(									\
		echo 'use "mlton-polyml.use";';					\
		echo 'PolyML.export("mlton-polyml", Main.mainWrapped);';	\
	) | "$(POLYML)"
	$(CC) -o mlton-polyml mlton-polyml.o -lpolymain -lpolyml
	rm -f mlton-polyml.o

mlton-polyml.use: ../lib/stubs/basis-stubs-for-polyml/sources.use ../lib/stubs/mlton-stubs-for-polyml/sources.use $(shell mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(filter-out control/version.sml,$(shell mlton -stop f mlton.mlb))
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
			grep -v 'targets' | 					\
			grep -v 'mlton-stubs' |					\
			grep -v 'mlyacc' |					\
			grep -v 'call-main.sml';				\
	) | sed 's|\(.*\)|use "\1";|' > mlton-polyml.use
