## Copyright (C) 2010,2013,2016,2018 Matthew Fluet.
 # Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

MLTON_RUNTIME_ARGS :=
MLTON_COMPILE_ARGS :=

BOOTSTRAP_MLTON_RUNTIME_ARGS :=
BOOTSTRAP_MLTON_COMPILE_ARGS :=

GIT := git
GREP := grep
SED := sed

######################################################################
######################################################################

SRC := $(shell cd .. && pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin

PATH := $(BIN):$(shell echo $$PATH)

MLTON_VERSION := $(shell TZ=UTC $(GIT) log -n1 --date=format-local:"%Y%m%d.%H%M%S" --pretty=format:"%cd-g%h$$([ "$$($(GIT) status --porcelain 2> /dev/null)" ] && echo '-dirty')" 2> /dev/null || echo '????????')

HOST_ARCH := $(shell ../bin/host-arch)
HOST_OS := $(shell ../bin/host-os)
TARGET := self
TARGET_ARCH := $(HOST_ARCH)
TARGET_OS := $(HOST_OS)

######################################################################

ifeq (other, $(shell if [ ! -x "$(BIN)/mlton" ]; then echo other; fi))
BOOTSTRAP:=true
else
BOOTSTRAP:=false
endif

MLTON := mlton
BOOTSTRAP_MLTON := mlton

MLTON_COMPILE_TYPECHECK_ARGS :=
ifeq (false, $(BOOTSTRAP))
# Older versions of `mlton` may not support `sequenceNonUnit` or `warnUnused`.
MLTON_COMPILE_TYPECHECK_ARGS += -default-ann 'sequenceNonUnit warn'
MLTON_COMPILE_TYPECHECK_ARGS += -default-ann 'warnUnused true'
# MLTON_COMPILE_TYPECHECK_ARGS += -type-check true
endif

ifeq (false, $(BOOTSTRAP))
MLTON_ACTUAL := $(MLTON)
MLTON_RUNTIME_ARGS_ACTUAL := ram-slop 0.7 $(MLTON_RUNTIME_ARGS)
MLTON_COMPILE_ARGS_ACTUAL := $(MLTON_COMPILE_TYPECHECK_ARGS) $(MLTON_COMPILE_ARGS)
else
MLTON_ACTUAL := $(BOOTSTRAP_MLTON)
MLTON_RUNTIME_ARGS_ACTUAL := ram-slop 0.7 $(BOOTSTRAP_MLTON_RUNTIME_ARGS)
MLTON_COMPILE_ARGS_ACTUAL := $(MLTON_COMPILE_TYPECHECK_ARGS) $(BOOTSTRAP_MLTON_COMPILE_ARGS)
endif

ifeq (false, $(BOOTSTRAP))
# We're self-compiling, so don't use any stubs.
FILE := mlton.mlb
else
ifneq (, $(findstring $(HOST_OS),cygwin mingw))
# We're bootstrapping on Cygwin or MinGW; stubs define `spawn` in terms of
# `fork` and `fork` doesn't work on Cygwin or MinGW, so don't use any stubs.
FILE := mlton.mlb
else
# We're bootstrapping, so use stubs.
FILE := mlton-stubs.mlb
endif
endif

FRONT_END_SOURCES :=		\
	front-end/ml.lex.sml	\
	front-end/ml.grm.sig	\
	front-end/ml.grm.sml	\
	front-end/mlb.lex.sml	\
	front-end/mlb.grm.sig	\
	front-end/mlb.grm.sml

SOURCES :=			\
	$(FILE)			\
	$(FRONT_END_SOURCES)	\
	$(filter-out control/version.sml,$(shell if [ -r $(FILE) ]; then mlton -stop f $(FILE) | $(GREP) -v " "; fi))

.PHONY: all
all: $(MLTON_OUTPUT)

control/version.sml: control/version_sml.src
	$(SED) "s/MLTON_VERSION/$(MLTON_VERSION)/" \
		< control/version_sml.src \
		> control/version.sml

front-end/%.lex.sml: front-end/%.lex
	$(MAKE) -C front-end $(<F).sml
front-end/%.grm.sig front-end/%.grm.sml: front-end/%.grm
	$(MAKE) -C front-end $(<F).sig $(<F).sml

mlton-stubs.mlb: $(shell mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(shell mlton -stop f mlton.mlb)
	(									\
		echo '$$(SML_LIB)/basis/unsafe.mlb';				\
		echo '$$(SML_LIB)/basis/sml-nj.mlb';				\
		echo '$$(SML_LIB)/basis/mlton.mlb';				\
		echo '$$(SML_LIB)/basis/basis.mlb';				\
		mlton -stop f mlton.mlb | $(GREP) -v 'mlb$$' | $(GREP) 'mlyacc'; \
		mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb |		\
			$(GREP) -v 'mlb$$' | 					\
			$(GREP) 'mlton-stubs';					\
		mlton -stop f mlton.mlb |					\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) -v 'sml/basis' | 				\
			$(GREP) -v 'targets' | 					\
			$(GREP) -v 'mlyacc';					\
	) > mlton-stubs.mlb

MLTON_OUTPUT := mlton
$(MLTON_OUTPUT): $(SOURCES)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	@echo 'Compiling mlton'
	"$(MLTON_ACTUAL)" @MLton gc-summary $(MLTON_RUNTIME_ARGS_ACTUAL) -- \
		-verbose 2 $(MLTON_COMPILE_ARGS_ACTUAL) \
		-target $(TARGET) -output $(MLTON_OUTPUT) $(FILE)

.PHONY: def-use
def-use: mlton.def-use

mlton.def-use: $(SOURCES)
	"$(MLTON_ACTUAL)" @MLton $(MLTON_RUNTIME_ARGS_ACTUAL) -- \
		-verbose 0 $(MLTON_COMPILE_ARGS_ACTUAL) \
		-stop tc -prefer-abs-paths true -show-def-use mlton.def-use \
		$(FILE)

.PHONY: clean
clean:
	../bin/clean

#
# The following rebuilds the heap file for the SML/NJ compiled version of MLton.
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
# The following rebuilds the executable file for the Poly/ML compiled version of
# MLton.
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
			$(SED) 's|use "\(.*\)";|../lib/stubs/basis-stubs-for-polyml/\1|'; \
		mlton -stop f mlton.mlb | $(GREP) -v 'mlb$$' | $(GREP) 'mlyacc'; \
		cat ../lib/stubs/mlton-stubs-for-polyml/sources.use |		\
			$(SED) 's|use "\(.*\)";|../lib/stubs/mlton-stubs-for-polyml/\1|'; \
		mlton -stop f ../lib/stubs/mlton-stubs/sources.mlb |		\
			$(GREP) -v 'mlb$$' | 					\
			$(GREP) 'mlton-stubs';					\
		mlton -stop f mlton.mlb |					\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) -v 'sml/basis' | 				\
			$(GREP) -v 'targets' | 					\
			$(GREP) -v 'mlton-stubs' |				\
			$(GREP) -v 'mlyacc' |					\
			$(GREP) -v 'call-main.sml';				\
	) | $(SED) 's|\(.*\)|use "\1";|' > mlton-polyml.use
