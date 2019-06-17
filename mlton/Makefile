## Copyright (C) 2010,2013,2016,2018-2019 Matthew Fluet.
 # Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

RUN_MLTON_RUNTIME_XARGS := ram-slop 0.7
RUN_MLTON_COMPILE_XARGS :=

ifeq (true, $(SELF_COMPILE))
# Older versions of `mlton` may not support `sequenceNonUnit` or `warnUnused`.
RUN_MLTON_COMPILE_XARGS += -default-ann 'sequenceNonUnit warn'
RUN_MLTON_COMPILE_XARGS += -default-ann 'warnUnused true'
# RUN_MLTON_COMPILE_XARGS += -type-check true
# We're self-compiling, so don't use any stubs.
MLTON_MLB := mlton.mlb
else
ifneq (, $(findstring $(HOST_OS),cygwin mingw))
# We're bootstrapping on Cygwin or MinGW; stubs define `spawn` in terms of
# `fork` and `fork` doesn't work on Cygwin or MinGW, so don't use any stubs.
MLTON_MLB := mlton.mlb
else
# We're bootstrapping, so use stubs.
MLTON_MLB := mlton-stubs.mlb
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
	$(MLTON_MLB)		\
	$(FRONT_END_SOURCES)	\
	$(filter-out control/version.sml,$(shell if [ -r $(MLTON_MLB) ]; then "$(RUN_MLTON)" -stop f $(MLTON_MLB); fi))

.PHONY: all
all: $(MLTON_OUTPUT)

.PHONY: clean
clean:
	../bin/clean

SHOW_VARS += MLTON_MLB
$(eval $(MK_SHOW_CONFIG))

######################################################################


$(MLTON_OUTPUT): $(SOURCES)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	@echo 'Compiling mlton'
	"$(RUN_MLTON)" \
		@MLton $(RUN_MLTON_RUNTIME_ARGS) $(RUN_MLTON_RUNTIME_XARGS) gc-summary -- \
		$(RUN_MLTON_COMPILE_ARGS) $(RUN_MLTON_COMPILE_XARGS) -verbose 2	\
		-target $(TARGET) -output $(MLTON_OUTPUT)			\
		$(MLTON_MLB)

control/version.sml: control/version_sml.src
	$(SED) \
		-e "s/MLTON_NAME/$(MLTON_NAME)/" \
		-e "s/MLTON_VERSION/$(MLTON_VERSION)/" \
		< control/version_sml.src \
		> control/version.sml

front-end/%.lex.sml: front-end/%.lex
	rm -f $<.*
	$(RUN_MLLEX) $<
	mv $<.sml $<.sml.in
	$(SED) -e 's/val s = List.map f (List.rev (tl (List.rev s)))/val s = Pervasive.List.map f (Pervasive.List.rev (tl (Pervasive.List.rev s)))/' $<.sml.in > $<.sml
	mv $<.sml $<.sml.in
	$(SED) -e 's/in Vector.fromList(List.map g/in Vector.fromList(Pervasive.List.map g/' $<.sml.in > $<.sml
	rm -f $<.sml.in
	chmod -w $<.*

front-end/%.grm.sig front-end/%.grm.sml: front-end/%.grm
	rm -f $<.*
	$(RUN_MLYACC) $<
	mv $<.sml $<.sml.in
	$(SED) -e 's/in f 0 handle General.Subscript => ()/in f 0 handle Pervasive.General.Subscript => ()/' $<.sml.in > $<.sml
	mv $<.sml $<.sml.in
	$(SED) -e 's/in Array.fromList(List.map actionRowLookUp actionRowNumbers)/in Array.fromList(Pervasive.List.map actionRowLookUp actionRowNumbers)/' $<.sml.in > $<.sml
	rm -f $<.sml.in
	chmod -w $<.*

mlton-stubs.mlb: $(shell "$(RUN_MLTON)" -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(shell "$(RUN_MLTON)" -stop f mlton.mlb)
	(									\
		echo '$$(SML_LIB)/basis/unsafe.mlb';				\
		echo '$$(SML_LIB)/basis/sml-nj.mlb';				\
		echo '$$(SML_LIB)/basis/mlton.mlb';				\
		echo '$$(SML_LIB)/basis/basis.mlb';				\
		$(RUN_MLTON) -stop f mlton.mlb |				\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) 'mlyacc';					\
		$(RUN_MLTON) -stop f ../lib/stubs/mlton-stubs/sources.mlb |	\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) 'mlton-stubs';					\
		$(RUN_MLTON) -stop f mlton.mlb |				\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) -v 'sml/basis' |				\
			$(GREP) -v 'targets' |					\
			$(GREP) -v 'mlyacc';					\
	) > mlton-stubs.mlb

######################################################################

.PHONY: def-use
def-use: mlton.def-use

mlton.def-use: $(SOURCES)
	"$(RUN_MLTON)" \
		@MLton $(RUN_MLTON_RUNTIME_ARGS) $(RUN_MLTON_RUNTIME_XARGS) --	\
		$(RUN_MLTON_COMPILE_ARGS) $(RUN_MLTON_COMPILE_XARGS) -verbose 0	\
		-stop tc -prefer-abs-paths true -show-def-use mlton.def-use	\
		$(MLTON_MLB)

######################################################################

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
		echo 'SMLofNJ.exportFn("$(MLTON_OUTPUT)-smlnj",Main.main);'	\
	) | "$(SMLNJ)"

######################################################################

#
# The following rebuilds the executable file for the Poly/ML compiled version of
# MLton.
#
POLYML := poly

.PHONY: polyml-mlton
polyml-mlton: mlton-polyml.use $(FRONT_END_SOURCES)
	rm -f control/version.sml
	$(MAKE) control/version.sml
	(									\
		echo 'use "mlton-polyml.use";';					\
		echo 'PolyML.export("$(MLTON_OUTPUT)-polyml", Main.mainWrapped);'; \
	) | "$(POLYML)"
	$(CC) -o $(MLTON_OUTPUT)-polyml $(MLTON_OUTPUT)-polyml.o -lpolymain -lpolyml
	rm -f $(MLTON_OUTPUT)-polyml.o

mlton-polyml.use: ../lib/stubs/basis-stubs-for-polyml/sources.use ../lib/stubs/mlton-stubs-for-polyml/sources.use $(shell "$(RUN_MLTON)" -stop f ../lib/stubs/mlton-stubs/sources.mlb) $(filter-out control/version.sml,$(shell  "$(RUN_MLTON)" -stop f mlton.mlb))
	(									\
		cat ../lib/stubs/basis-stubs-for-polyml/sources.use |		\
			$(SED) 's|use "\(.*\)";|../lib/stubs/basis-stubs-for-polyml/\1|'; \
		"$(RUN_MLTON)" -stop f mlton.mlb | $(GREP) -v 'mlb$$' | $(GREP) 'mlyacc'; \
		cat ../lib/stubs/mlton-stubs-for-polyml/sources.use |		\
			$(SED) 's|use "\(.*\)";|../lib/stubs/mlton-stubs-for-polyml/\1|'; \
		"$(RUN_MLTON)" -stop f ../lib/stubs/mlton-stubs/sources.mlb |	\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) 'mlton-stubs';					\
		"$(RUN_MLTON)" -stop f mlton.mlb |				\
			$(GREP) -v 'mlb$$' |					\
			$(GREP) -v 'sml/basis' |				\
			$(GREP) -v 'targets' |					\
			$(GREP) -v 'mlton-stubs' |				\
			$(GREP) -v 'mlyacc' |					\
			$(GREP) -v 'call-main.sml';				\
	) | $(SED) 's|\(.*\)|use "\1";|' > mlton-polyml.use
