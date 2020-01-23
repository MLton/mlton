## Copyright (C) 2009,2011,2013,2017-2020 Matthew Fluet.
 # Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := .
include $(ROOT)/Makefile.config

######################################################################

.PHONY: all
all:
	$(MAKE) dirs
	$(MAKE) runtime
	$(MAKE) compiler CHECK_FIXPOINT=false                     # tools0 + mlton0 -> mlton1
	$(MAKE) script
	$(MAKE) basis
	$(MAKE) libraries
	$(MAKE) tools    CHECK_FIXPOINT=false                     # tools0 + mlton1 -> tools1
ifeq (2, $(firstword $(sort $(BOOTSTRAP_STYLE) 2)))
	$(MAKE) compiler-clean
	$(MAKE) compiler SELF_COMPILE=true  CHECK_FIXPOINT=false  # tools1 + mlton1 -> mlton2
ifeq (3, $(firstword $(sort $(BOOTSTRAP_STYLE) 3)))
	$(MAKE) tools-clean
	$(MAKE) tools    CHECK_FIXPOINT=true                      # tools1 + mlton2 -> tools2; tools2 == tools1
	$(MAKE) compiler-clean
	$(MAKE) compiler SELF_COMPILE=true  CHECK_FIXPOINT=true   # tools2 + mlton2 -> mlton3; mlton3 == mlton2
endif
endif
	@echo 'Build of MLton succeeded.'

.PHONY: clean
clean:
	$(SRC)/bin/clean --exclude package

.PHONY: clean-git
clean-git:
	$(FIND) . -type d -name .git -prune -exec $(RM) '{}' ';'

.PHONY: check
check:
	./bin/regression $(CHECK_ARGS)

$(eval $(MK_SHOW_CONFIG))

######################################################################

.PHONY: basis-no-check
basis-no-check:
	$(RM) "$(LIB)/sml/basis"
	$(MKDIR) "$(LIB)/sml/basis"
	( \
	cd "$(SRC)/basis-library" && \
	$(FIND) . -type f '(' -name '*.mlb' -o -name '*.sml' -o -name '*.sig' -o -name '*.fun' ')' | \
	$(XARGS) $(TAR) cf - | \
	( cd "$(LIB)/sml/basis" && $(TAR) xf - ) \
	)

.PHONY: basis-check
basis-check:
	@echo 'Type checking basis.'
	"$(BIN)/$(MLTON)" -disable-ann deadCode -stop tc '$$(SML_LIB)/basis/libs/all.mlb' >/dev/null

.PHONY: basis
basis:
	$(MAKE) basis-no-check
	$(MAKE) basis-check

.PHONY: compiler
compiler:
	$(MAKE) -C "$(SRC)/mlton"
ifeq (true, $(CHECK_FIXPOINT))
	$(DIFF) -b "$(SRC)/mlton/$(MLTON_OUTPUT)$(EXE)" "$(LIB)/$(MLTON_OUTPUT)$(EXE)"
endif
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT)$(EXE)" "$(LIB)/"

.PHONY: compiler-clean
compiler-clean:
	$(MAKE) -C "$(SRC)/mlton" clean

.PHONY: dirs
dirs:
	$(MKDIR) "$(BIN)" "$(LIB)" "$(INC)"
	$(MKDIR) "$(LIB)/targets/$(TARGET)/include"
	$(MKDIR) "$(LIB)/targets/$(TARGET)/sml"

.PHONY: docs
docs:
	$(MAKE) -C "$(SRC)/mllex" docs
	$(MAKE) -C "$(SRC)/mlyacc" docs
	$(MAKE) -C "$(SRC)/doc/guide"

define LIBRARIES_NO_CHECK_TEMPLATE
	$(RM) "$(LIB)/sml/$(1)"
	$(MKDIR) "$(LIB)/sml/$(1)"
	( \
	cd "$(SRC)/lib/$(1)$(2)" && \
	$(FIND) . '!' -path '*/.cm/*' $(3) -type f '(' -name '*.mlb' -o -name '*.sml' -o -name '*.sig' -o -name '*.fun' ')' | \
	$(XARGS) $(TAR) cf - | \
	( cd "$(LIB)/sml/$(1)" && $(TAR) xf - ) \
	)

endef

.PHONY: libraries-no-check
libraries-no-check:
	$(MAKE) -C "$(SRC)/lib/ckit-lib"
	$(call LIBRARIES_NO_CHECK_TEMPLATE,ckit-lib,/ckit/src,)
	$(call LIBRARIES_NO_CHECK_TEMPLATE,cml,,'!' -path '*/tests/*')
	$(MAKE) -C "$(SRC)/lib/mllpt-lib"
	$(call LIBRARIES_NO_CHECK_TEMPLATE,mllpt-lib,/ml-lpt/lib,)
	$(MAKE) -C "$(SRC)/lib/mlnlffi-lib"
	$(call LIBRARIES_NO_CHECK_TEMPLATE,mlnlffi-lib,,)
	$(MAKE) -C "$(SRC)/lib/mlrisc-lib"
	$(call LIBRARIES_NO_CHECK_TEMPLATE,mlrisc-lib,/MLRISC,'!' -path '*/demo/*' '!' -path '*/Tools/*' '!' -path './autoload.sml' '!' -path './make*.sml')
	$(call LIBRARIES_NO_CHECK_TEMPLATE,mlyacc-lib,,)
	$(MAKE) -C "$(SRC)/lib/smlnj-lib"
	$(call LIBRARIES_NO_CHECK_TEMPLATE,smlnj-lib,/smlnj-lib,'!' -path '*/examples/*' '!' -path '*/tests/*' '!' -path '*/Tests/*')

define LIBRARIES_CHECK_TEMPLATE
	@echo "Type checking $(1) library."
	"$(BIN)/$(MLTON)" -disable-ann deadCode -stop tc '$$(SML_LIB)/$(1)/$(1).mlb' >/dev/null
endef

.PHONY: libraries-check
libraries-check:
	$(call LIBRARIES_CHECK_TEMPLATE,ckit-lib)
	$(call LIBRARIES_CHECK_TEMPLATE,cml)
	$(call LIBRARIES_CHECK_TEMPLATE,mllpt-lib)
	$(call LIBRARIES_CHECK_TEMPLATE,mlnlffi-lib)
	$(call LIBRARIES_CHECK_TEMPLATE,mlrisc-lib)
	$(call LIBRARIES_CHECK_TEMPLATE,mlyacc-lib)
	$(call LIBRARIES_CHECK_TEMPLATE,smlnj-lib)

.PHONY: libraries
libraries:
	$(MAKE) libraries-no-check
	$(MAKE) libraries-check

.PHONY: runtime
runtime:
	@echo 'Compiling MLton runtime system for $(TARGET).'
	$(MAKE) -C "$(SRC)/runtime"
	$(CP) "$(SRC)/include/"*.h "$(INC)/"
	$(CP) "$(SRC)/runtime/"*.a "$(LIB)/targets/$(TARGET)/"
	$(CP) "$(SRC)/runtime/gen/constants" "$(LIB)/targets/$(TARGET)/"
	$(CP) "$(SRC)/runtime/gen/c-types.sml" "$(LIB)/targets/$(TARGET)/sml/"
	echo "$(TARGET_OS)" > "$(LIB)/targets/$(TARGET)/os"
	echo "$(TARGET_ARCH)" > "$(LIB)/targets/$(TARGET)/arch"
	$(CP) "$(SRC)/runtime/gen/basis-ffi.sml" \
		basis-library/primitive/basis-ffi.sml
	$(CP) "$(SRC)/runtime/"*.h "$(INC)/"
	$(MV) "$(INC)/c-types.h" "$(LIB)/targets/$(TARGET)/include"
	for d in basis basis/Real basis/Word gc platform util; do	\
		$(MKDIR) "$(INC)/$$d";					\
		$(CP) "$(SRC)/runtime/$$d/"*.h "$(INC)/$$d";		\
	done

.PHONY: script
script:
	$(SED) \
		-e "s;^LIB_REL_BIN=.*;LIB_REL_BIN=\"$(LIB_REL_BIN)\";" \
		-e "s;^EXE=.*;EXE=\"$(EXE)\";" \
		-e "s;^CC=.*;CC=\"$(CC)\";" \
		-e "s;^GMP_INC_DIR=.*;GMP_INC_DIR=\"$(WITH_GMP_INC_DIR)\";" \
		-e "s;^GMP_LIB_DIR=.*;GMP_LIB_DIR=\"$(WITH_GMP_LIB_DIR)\";" \
		-e 's/mlton-compile/$(MLTON_OUTPUT)/' \
		< "$(SRC)/bin/mlton-script" > "$(BIN)/$(MLTON)"
	chmod a+x "$(BIN)/$(MLTON)"
	$(CP) "$(SRC)/bin/static-library" "$(LIB)"
ifeq (mingw, $(TARGET_OS))
	$(CP) "$(SRC)/bin/static-library.bat" "$(LIB)"
endif

ifeq (true, $(CHECK_FIXPOINT))
define TOOLS_TEMPLATE_CHECK_FIXPOINT
	$(DIFF) -b "$(SRC)/$(1)/$(1)$(EXE)" "$(BIN)/$(1)$(EXE)"
endef
else
define TOOLS_TEMPLATE_CHECK_FIXPOINT
endef
endif

define TOOLS_TEMPLATE
	$(MAKE) -C "$(SRC)/$(1)"
	$(call TOOLS_TEMPLATE_CHECK_FIXPOINT,$(1))
	$(CP) "$(1)/$(1)$(EXE)" "$(BIN)/"
endef

.PHONY: tools
tools:
	$(call TOOLS_TEMPLATE,mllex)
	$(call TOOLS_TEMPLATE,mlyacc)
	$(call TOOLS_TEMPLATE,mlprof)
	$(call TOOLS_TEMPLATE,mlnlffigen)

.PHONY: tools-clean
tools-clean:
	$(MAKE) -C "$(SRC)/mllex" clean
	$(MAKE) -C "$(SRC)/mlyacc" clean
	$(MAKE) -C "$(SRC)/mlprof" clean
	$(MAKE) -C "$(SRC)/mlnlffigen" clean

######################################################################

# debugged, profiled, traced targets

.PHONY: debugged
debugged:
	$(MAKE) -C "$(SRC)/mlton" MLTON_NAME="$(MLTON_NAME) (debug)" MLTON_OUTPUT=$(MLTON_OUTPUT).debug \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -debug true -const 'Exn.keepHistory true' -profile-val true -const 'MLton.debug true' -disable-pass 'deepFlatten'"
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT).debug$(EXE)" "$(LIB)/"
	$(SED) -e 's/$(MLTON_OUTPUT)/$(MLTON_OUTPUT).debug/' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).debug"
	chmod u+x "$(BIN)/$(MLTON).debug"

define PROFILED_TEMPLATE
	$(MAKE) -C "$(SRC)/mlton" MLTON_NAME="$(MLTON_NAME) (profile-$(1))" MLTON_OUTPUT=$(MLTON_OUTPUT).$(1) \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -profile $(1)"
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT).$(1)$(EXE)" "$(LIB)/"
	$(SED) -e "s/$(MLTON_OUTPUT)/$(MLTON_OUTPUT).$(1)/" \
		< "$(BIN)/$(MLTON)" \
		>"$(BIN)/$(MLTON).$(1)"
	chmod u+x "$(BIN)/$(MLTON).$(1)"
endef

.PHONY: profiled-alloc
profiled-alloc:
	$(call PROFILED_TEMPLATE,alloc)

.PHONY: profiled-count
profiled-count:
	$(call PROFILED_TEMPLATE,count)

.PHONY: profiled-time
profiled-time:
	$(call PROFILED_TEMPLATE,time)

.PHONY: profiled
profiled:
	$(MAKE) profiled-alloc
	$(MAKE) profiled-count
	$(MAKE) profiled-time

.PHONY: traced
traced:
	$(MAKE) -C "$(SRC)/mlton" MLTON_NAME="$(MLTON_NAME) (trace)" MLTON_OUTPUT=$(MLTON_OUTPUT).trace \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -const 'Exn.keepHistory true' -profile-val true -const 'MLton.debug true' -disable-pass 'deepFlatten'"
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT).trace$(EXE)" "$(LIB)/"
	$(SED) -e 's/$(MLTON_OUTPUT)/$(MLTON_OUTPUT).trace/' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).trace"
	chmod u+x "$(BIN)/$(MLTON).trace"

######################################################################

# smlnj targets

.PHONY: bootstrap-smlnj
bootstrap-smlnj:
	$(MAKE) smlnj-mlton
	$(RM) "$(BIN)/$(MLTON)"
	$(MAKE) BOOTSTRAP_MLTON=$(MLTON).smlnj all
	smlnj_heap_suffix=`echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null` && $(RM) "$(LIB)/$(MLTON_OUTPUT)-smlnj.$$smlnj_heap_suffix"
	$(RM) "$(BIN)/$(MLTON).smlnj"

.PHONY: smlnj-mlton
smlnj-mlton:
	$(MAKE) dirs
	$(MAKE) runtime
	$(MAKE) -C "$(SRC)/mlton" smlnj-mlton
	smlnj_heap_suffix=`echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null` && $(CP) "$(SRC)/mlton/$(MLTON_OUTPUT)-smlnj.$$smlnj_heap_suffix" "$(LIB)/"
	$(MAKE) script
	$(MAKE) basis-no-check
	$(MAKE) libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitPolyML "$$@";# doitPolyML "$$@";' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).smlnj"
	chmod u+x "$(BIN)/$(MLTON).smlnj"
	@echo 'Build of MLton (with SML/NJ) succeeded.'

.PHONY: smlnj-mlton-x2
smlnj-mlton-x2:
	$(MAKE) SMLNJ_CM_SERVERS_NUM=2 smlnj-mlton

.PHONY: smlnj-mlton-x4
smlnj-mlton-x4:
	$(MAKE) SMLNJ_CM_SERVERS_NUM=4 smlnj-mlton

.PHONY: smlnj-mlton-x8
smlnj-mlton-x8:
	$(MAKE) SMLNJ_CM_SERVERS_NUM=8 smlnj-mlton

.PHONY: smlnj-mlton-x16
smlnj-mlton-x16:
	$(MAKE) SMLNJ_CM_SERVERS_NUM=16 smlnj-mlton

######################################################################

# polyml targets

.PHONY: bootstrap-polyml
bootstrap-polyml:
	$(MAKE) polyml-mlton
	$(RM) "$(BIN)/$(MLTON)"
	$(MAKE) BOOTSTRAP_MLTON=$(MLTON).polyml all
	$(RM) "$(LIB)/$(MLTON)-polyml$(EXE)"
	$(RM) "$(BIN)/$(MLTON).polyml"

.PHONY: polyml-mlton
polyml-mlton:
	$(MAKE) dirs
	$(MAKE) runtime
	$(MAKE) -C "$(SRC)/mlton" polyml-mlton
	$(CP) "$(SRC)/mlton/mlton-polyml$(EXE)" "$(LIB)/"
	$(MAKE) script
	$(MAKE) basis-no-check
	$(MAKE) libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitSMLNJ "$$@";# doitSMLNJ "$$@";' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).polyml"
	chmod u+x "$(BIN)/$(MLTON).polyml"
	@echo 'Build of MLton (with Poly/ML) succeeded.'

######################################################################

# version target

.PHONY: version
version:
	@echo 'Instantiating version numbers.'
	$(SED) \
		-e "s/^MLTON_VERSION := .*/MLTON_VERSION := $(MLTON_VERSION)/" \
		-e "s/^RELEASE := .*/RELEASE := true/" \
		< "$(SRC)/Makefile.config" \
		> z && \
		$(MV) z "$(SRC)/Makefile.config";

######################################################################

# install and release targets

prefix := $(PREFIX)
exec_prefix := $(prefix)
bindir := $(exec_prefix)/bin
datarootdir := $(prefix)/share
docdir := $(datarootdir)/doc/mlton
libdir := $(exec_prefix)/lib
mandir := $(datarootdir)/man
man1dir := $(mandir)/man1

TBIN := $(DESTDIR)$(bindir)
TLIB := $(DESTDIR)$(libdir)/mlton
TMAN := $(DESTDIR)$(man1dir)
TDOC := $(DESTDIR)$(docdir)
TEXM := $(TDOC)/examples

TLIB_REL_TBIN := $(shell $(call TGT_REL_SRC,$(TLIB),$(TBIN)))

GZIP_MAN := true
ifeq ($(findstring $(TARGET_OS), openbsd solaris), $(TARGET_OS))
GZIP_MAN := false
endif

.PHONY: install
install: install-no-strip install-strip

MAN_PAGES :=  \
	mllex.1 \
	mlnlffigen.1 \
	mlprof.1 \
	mlton.1 \
	mlyacc.1

.PHONY: install-no-strip
install-no-strip:
	$(MKDIR) "$(TBIN)" "$(TLIB)" "$(TMAN)"
	$(CP) "$(BIN)/." "$(TBIN)/"
	$(SED) \
		-e "s;^LIB_REL_BIN=.*;LIB_REL_BIN=\"$(TLIB_REL_TBIN)\";" \
		< "$(BIN)/$(MLTON)" > "$(TBIN)/$(MLTON)"
	chmod a+x "$(TBIN)/$(MLTON)"
	$(CP) "$(LIB)/." "$(TLIB)/"
	cd "$(SRC)/man" && $(CP) $(MAN_PAGES) "$(TMAN)/"
ifeq (true, $(GZIP_MAN))
	cd "$(TMAN)" && $(GZIP) --force --best $(MAN_PAGES);
endif

.PHONY: install-strip
install-strip: install-no-strip
	for f in "$(TLIB)/$(MLTON_OUTPUT)$(EXE)" 		\
		"$(TBIN)/mllex$(EXE)"				\
		"$(TBIN)/mlyacc$(EXE)"				\
		"$(TBIN)/mlprof$(EXE)" 				\
		"$(TBIN)/mlnlffigen$(EXE)"; do			\
		$(STRIP) "$$f";					\
	done

REGRESSION_EXAMPLES := \
	callcc.sml command-line.sml hello-world.sml same-fringe.sml	\
	signals.sml size.sml taut.sml thread1.sml thread2.sml		\
	thread-switch.sml timeout.sml

.PHONY: install-docs
install-docs:
	$(MKDIR) "$(TDOC)" "$(TDOC)/license"
	(								\
		cd "$(SRC)" &&						\
		$(CP) CHANGELOG.adoc README.adoc "$(TDOC)/" &&		\
		$(CP) LICENSE "$(TDOC)/license/MLton-LICENSE"		\
	)
	(								\
		cd "$(SRC)/doc" &&					\
		$(FIND) examples -type f '!' -name .gitignore		\
			| $(XARGS) $(TAR) cf -				\
			| ( cd "$(TDOC)/" && $(TAR) xf - )		\
	)
	(								\
		cd "$(SRC)/doc" &&					\
		$(FIND) license -type f '!' -name .gitignore		\
			| $(XARGS) $(TAR) cf -				\
			| ( cd "$(TDOC)/" && $(TAR) xf - )		\
	)
	if [ -d "$(SRC)/doc/guide/localhost" ]; then			\
		$(CP) "$(SRC)/doc/guide/localhost" "$(TDOC)/guide";	\
	fi
	if [ -r "$(SRC)/doc/guide/mlton-guide.pdf" ]; then		\
		$(CP) "$(SRC)/doc/guide/mlton-guide.pdf" "$(TDOC)/";	\
	fi
	if [ -r "mllex/mllex.pdf" ]; then				\
		$(CP) "mllex/mllex.pdf" "$(TDOC)/";			\
	fi
	if [ -r "mlyacc/mlyacc.pdf" ]; then				\
		$(CP) "mlyacc/mlyacc.pdf" "$(TDOC)/";			\
	fi
	(								\
		cd "$(SRC)/util" &&					\
		$(FIND) cm2mlb -type f '!' -name .gitignore		\
			| $(XARGS) $(TAR) cf -				\
			| ( cd "$(TDOC)/" && $(TAR) xf - )		\
	)
	(								\
		cd "$(SRC)/regression" &&				\
		$(CP) $(REGRESSION_EXAMPLES) "$(TEXM)/"			\
	)


.PHONY: source-release
source-release:
	$(MAKE) clean
	$(MAKE) MLTON_VERSION=$(MLTON_VERSION) version
	( cd "$(SRC)/mllex" ; latexmk -pdf lexgen ; latexmk -c lexgen )
	$(MAKE) -C "$(SRC)/mllex" mllex.pdf
	( cd "$(SRC)/mlyacc/doc"; latexmk -pdf mlyaccc ; latexmk -c mlyacc )
	$(MAKE) -C "$(SRC)/mlyacc" mlyacc.pdf
	$(MAKE) -C doc/guide
	$(TAR) cvzf ../mlton-$(MLTON_VERSION).src.tgz \
		--exclude .git --exclude package \
		--transform "s@^@mlton-$(MLTON_VERSION)/@S" \
		*

MLTON_BINARY_RELEASE := 1
MLTON_BINARY_RELEASE_SUFFIX :=
.PHONY: binary-release
binary-release:
	$(MAKE) all
	$(MAKE) docs
	$(RM) "$(SRC)/mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)"
	$(MKDIR) "$(SRC)/mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)"
	$(MAKE) DESTDIR="$(SRC)/mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)" PREFIX="" install
	$(CP) "$(SRC)/Makefile.binary" "$(SRC)/mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)/Makefile"
	$(CP) "$(SRC)/CHANGELOG.adoc" "$(SRC)/LICENSE" "$(SRC)/README.adoc" "$(SRC)/mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)/"
	$(TAR) cvzf ../mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX).tgz \
		mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)
	$(RM) mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)

BSDSRC := /tmp/mlton-$(MLTON_VERSION)
MLTON_FREEBSD_RELEASE := 1
.PHONY: freebsd
freebsd:
	$(MAKE) clean clean-git version
	$(RM) "$(BSDSRC)"
	$(MKDIR) "$(BSDSRC)"
	( cd $(SRC) && tar -cpf - . ) | ( cd "$(BSDSRC)" && tar -xpf - )
	cd /tmp && tar -cpf - mlton-$(MLTON_VERSION) | \
		 $(GZIP) --force --best >/usr/ports/distfiles/mlton-$(MLTON_VERSION)-$(MLTON_FREEBSD_RELEASE).freebsd.src.tgz
        # do not change "make" to "$(MAKE)" in the following line
	cd "$(BSDSRC)/package/freebsd" && MAINTAINER_MODE=yes make build-package
