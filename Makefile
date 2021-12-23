## Copyright (C) 2009,2011,2013,2017-2021 Matthew Fluet.
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
	$(TAR) -c -T - -f - | \
	( cd "$(LIB)/sml/basis" && $(TAR) -x -f - ) \
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
	$(MAKE) -C "$(SRC)/doc/guide"

define LIBRARIES_NO_CHECK_TEMPLATE
	$(RM) "$(LIB)/sml/$(1)"
	$(MKDIR) "$(LIB)/sml/$(1)"
	( \
	cd "$(SRC)/lib/$(1)$(2)" && \
	$(FIND) . '!' -path '*/.cm/*' $(3) -type f '(' -name '*.mlb' -o -name '*.sml' -o -name '*.sig' -o -name '*.fun' ')' | \
	$(TAR) -c -T - -f - | \
	( cd "$(LIB)/sml/$(1)" && $(TAR) -x -f - ) \
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
		-e "s;^    SMLNJ=.*;    SMLNJ=\"$(SMLNJ)\";" \
		< "$(SRC)/bin/mlton-script" > "$(BIN)/$(MLTON)"
	$(CHMOD) a+x "$(BIN)/$(MLTON)"
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
	+$(MAKE) -C "$(SRC)/$(1)"
	$(call TOOLS_TEMPLATE_CHECK_FIXPOINT,$(1))
	$(CP) "$(SRC)/$(1)/$(1)$(EXE)" "$(BIN)/"
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
	$(CHMOD) u+x "$(BIN)/$(MLTON).debug"

define PROFILED_TEMPLATE
	+$(MAKE) -C "$(SRC)/mlton" MLTON_NAME="$(MLTON_NAME) (profile-$(1))" MLTON_OUTPUT=$(MLTON_OUTPUT).$(1) \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -profile $(1)"
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT).$(1)$(EXE)" "$(LIB)/"
	$(SED) -e "s/$(MLTON_OUTPUT)/$(MLTON_OUTPUT).$(1)/" \
		< "$(BIN)/$(MLTON)" \
		>"$(BIN)/$(MLTON).$(1)"
	$(CHMOD) u+x "$(BIN)/$(MLTON).$(1)"
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
	$(CHMOD) u+x "$(BIN)/$(MLTON).trace"

######################################################################

# smlnj targets

SMLNJ := sml

.PHONY: bootstrap-smlnj
bootstrap-smlnj:
	$(MAKE) smlnj-mlton
	$(RM) "$(BIN)/$(MLTON)"
	$(MAKE) OLD_MLTON="$(BIN)/$(MLTON).smlnj" all
	$(RM) "$(LIB)/$(MLTON_OUTPUT_SMLNJ_HEAP)"
	$(RM) "$(BIN)/$(MLTON).smlnj"

.PHONY: smlnj-mlton
smlnj-mlton:
	$(MAKE) dirs
	$(MAKE) runtime
	$(MAKE) -C "$(SRC)/mlton" smlnj-mlton
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT_SMLNJ_HEAP)" "$(LIB)/"
	$(MAKE) script
	$(MAKE) basis-no-check
	$(MAKE) libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitPolyML "$$@";# doitPolyML "$$@";' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).smlnj"
	$(CHMOD) u+x "$(BIN)/$(MLTON).smlnj"
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
	$(MAKE) OLD_MLTON="$(BIN)/$(MLTON).polyml" all
	$(RM) "$(LIB)/$(MLTON)-polyml$(EXE)"
	$(RM) "$(BIN)/$(MLTON).polyml"

.PHONY: polyml-mlton
polyml-mlton:
	$(MAKE) dirs
	$(MAKE) runtime
	$(MAKE) -C "$(SRC)/mlton" polyml-mlton
	$(CP) "$(SRC)/mlton/$(MLTON_OUTPUT)-polyml$(EXE)" "$(LIB)/"
	$(MAKE) script
	$(MAKE) basis-no-check
	$(MAKE) libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitSMLNJ "$$@";# doitSMLNJ "$$@";' \
		< "$(BIN)/$(MLTON)" \
		> "$(BIN)/$(MLTON).polyml"
	$(CHMOD) u+x "$(BIN)/$(MLTON).polyml"
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

MAN_PAGES := mlton.1
MAN_PAGES := $(MAN_PAGES) \
	mllex.1 \
	mlnlffigen.1 \
	mlprof.1 \
	mlyacc.1

.PHONY: install-no-strip
install-no-strip:
	$(MKDIR) "$(TBIN)" "$(TLIB)"
	$(CP) "$(BIN)/." "$(TBIN)/"
	$(SED) \
		-e "s;^LIB_REL_BIN=.*;LIB_REL_BIN=\"$(TLIB_REL_TBIN)\";" \
		< "$(BIN)/$(MLTON)" > "$(TBIN)/$(MLTON)"
	$(CHMOD) a+x "$(TBIN)/$(MLTON)"
	$(CP) "$(LIB)/." "$(TLIB)/"
	$(MKDIR) "$(TMAN)"
	cd "$(SRC)/man" && $(CP) $(MAN_PAGES) "$(TMAN)/"
ifeq (true, $(GZIP_MAN))
	cd "$(TMAN)" && $(GZIP) --force --best $(MAN_PAGES);
endif

STRIP_PROGS := "$(TLIB)/$(MLTON_OUTPUT)$(EXE)"
STRIP_PROGS := $(STRIP_PROGS) \
	"$(TBIN)/mllex$(EXE)" \
	"$(TBIN)/mlnlffigen$(EXE)" \
	"$(TBIN)/mlprof$(EXE)" \
	"$(TBIN)/mlyacc$(EXE)"

.PHONY: install-strip
install-strip: install-no-strip
	for f in $(STRIP_PROGS); do $(STRIP) "$$f"; done

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
			| $(TAR) -c -T - f -				\
			| ( cd "$(TDOC)/" && $(TAR) -x -f - )		\
	)
	(								\
		cd "$(SRC)/doc" &&					\
		$(FIND) license -type f '!' -name .gitignore		\
			| $(TAR) -c -T - -f -				\
			| ( cd "$(TDOC)/" && $(TAR) -x -f - )		\
	)
	if [ -d "$(SRC)/doc/guide/localhost" ]; then			\
		$(CP) "$(SRC)/doc/guide/localhost" "$(TDOC)/guide";	\
	fi
	(								\
		cd "$(SRC)/util" &&					\
		$(FIND) cm2mlb -type f '!' -name .gitignore		\
			| $(TAR) -c -T - f -				\
			| ( cd "$(TDOC)/" && $(TAR) -x -f - )		\
	)
	(								\
		cd "$(SRC)/regression" &&				\
		$(CP) $(REGRESSION_EXAMPLES) "$(TEXM)/"			\
	)


.PHONY: source-release
source-release:
	$(MAKE) clean
	$(MAKE) MLTON_VERSION=$(MLTON_VERSION) version
	$(MAKE) -C doc/guide
	$(TAR) cvzf ../mlton-$(MLTON_VERSION).src.tgz \
		--exclude .git --exclude package \
		--transform "s@^@mlton-$(MLTON_VERSION)/@S" \
		*

MLTON_BINARY_RELEASE := 1
MLTON_BINARY_RELEASE_SUFFIX :=
MLTON_BINARY_RELEASE_NAME := mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(TARGET_ARCH)-$(TARGET_OS)$(MLTON_BINARY_RELEASE_SUFFIX)
MLTON_BINARY_RELEASE_WITH_DOCS := true
.PHONY: binary-release
binary-release:
	$(MAKE) all
ifeq (true,$(MLTON_BINARY_RELEASE_WITH_DOCS))
	$(MAKE) docs
endif
	$(RM) "$(SRC)/$(MLTON_BINARY_RELEASE_NAME)"
	$(MKDIR) "$(SRC)/$(MLTON_BINARY_RELEASE_NAME)"
	$(MAKE) DESTDIR="$(SRC)/$(MLTON_BINARY_RELEASE_NAME)" PREFIX="" install
ifeq (true,$(MLTON_BINARY_RELEASE_WITH_DOCS))
	$(MAKE) DESTDIR="$(SRC)/$(MLTON_BINARY_RELEASE_NAME)" PREFIX="" install-docs
endif
	$(CP) "$(SRC)/Makefile.binary" "$(SRC)/$(MLTON_BINARY_RELEASE_NAME)/Makefile"
	$(CP) "$(SRC)/CHANGELOG.adoc" "$(SRC)/LICENSE" "$(SRC)/README.adoc" "$(SRC)/$(MLTON_BINARY_RELEASE_NAME)/"
	$(TAR) cvzf $(MLTON_BINARY_RELEASE_NAME).tgz $(MLTON_BINARY_RELEASE_NAME)
	$(RM) $(MLTON_BINARY_RELEASE_NAME)

######################################################################

# remote-bootstrap and remote-add-cross-target

# The `remote-bootstrap` goal automates the process of bootstraping MLton on a
# remote machine that doesn't have a suitable pre-compiled `mlton` binary.  It
# works as follows:
#  * send the current sources to a remote machine (using ssh)
#  * build the MLton runtime system on the remote machine
#  * receive the built runtime system from the remote machine as a new target on
#    the host machine
#  * build bootstrap compiler sources on the host machine for the new target
#  * send the boostrap sources to the remote machine
#  * build the boostrap compiler on the remote machine using the boostrap
#    compiler sources
#  * complete the MLton build on the remote machine with the boostrap compiler to
#    obtain a boot package
#  * build MLton on the remote machine from clean sources using the boot package
#  * receive the built binary release from the remote machine
#
# The `remote-add-cross-target` goal automates the process of adding a cross
# compiler target.  It works as follows:
#  * send the current sources to a remote machine (using ssh)
#  * build the MLton runtime system on the remote machine
#  * receive the built runtime system from the remote machine as a new target on
#    the host machine
#
#
# For both `remote-bootstrap` and `remote-add-cross`, the following `Makefile`
# variables are used:
#  * REMOTE_MACHINE (required): Specify the remote machine to be used as an
#    `ssh` destination, either `[user@]hostname` or
#    `ssh://[user@]hostname[:port]`.
#  * REMOTE_TMP: Specify (absolute) path on remote machine for building; default
#    is `/tmp`.
#  * REMOTE_MAKE: Specify `make` on remote machine.
#  * REMOTE_MAKEFLAGS: Specify `Makefile` variables to set when invoking `make`
#    on the remote machine.
# For `remote-add-cross`, the following additional `Makefile` variable is used:
#  * CROSS_TARGET (required): Specify target machine triple for host
#    cross-compiler; that is the value of `<machine>` in `<machine>-gcc` or
#    `<triple>` in `clang -target=<triple>` on the host machine, such as
#    `arm-linux-gnu`.
#
#
# For example, on OpenBSD, GNU Make is named `gmake` (and `make` is BSD Make)
# and GMP is provided at `/usr/local` (and not on the default search paths).  To
# boostrap on an OpenBSD virtual machine (with localhost port 2222 forwarded to
# guest VM port 22), one could use:
#
# $ make REMOTE_MACHINE=ssh://localhost:2222 REMOTE_MAKE=gmake REMOTE_MAKEFLAGS=WITH_GMP_DIR=/usr/local remote-bootstrap

ifneq (,$(findstring remote-,$(MAKECMDGOALS)))

ifneq (,$(findstring dirty,$(MLTON_VERSION)))
$(warning 'MLTON_VERSION' appears dirty; $(MLTON_VERSION))
endif

ifeq (,$(REMOTE_MACHINE))
$(error 'REMOTE_MACHINE' not defined)
endif

SSH := ssh

ifeq (false,$(shell $(SSH) $(REMOTE_MACHINE) true > /dev/null 2>&1 || echo false))
$(error '$(SSH) $(REMOTE_MACHINE) true' failed)
endif

REMOTE_TMP := /tmp
REMOTE_ROOT := $(REMOTE_TMP)/mlton-$(MLTON_VERSION)

REMOTE_BASH := bash
REMOTE_TAR := $(TAR)

REMOTE_CAT := $(CAT)
REMOTE_CP := $(CP)
REMOTE_MKDIR := $(MKDIR)
REMOTE_RM := $(RM)

REMOTE_MAKE := make
REMOTE_MAKEFLAGS :=
REMOTE_XMAKEFLAGS := CHECK_MLCMD=

REMOTE_PLATFORM := $(shell $(CAT) bin/platform | $(SSH) $(REMOTE_MACHINE) "$(REMOTE_BASH) -s")
REMOTE_ARCH := $(patsubst HOST_ARCH=%,%,$(filter HOST_ARCH=%,$(REMOTE_PLATFORM)))
REMOTE_OS := $(patsubst HOST_OS=%,%,$(filter HOST_OS=%,$(REMOTE_PLATFORM)))
REMOTE_TARGET := $(REMOTE_ARCH)-$(REMOTE_OS)
CROSS_TARGET := $(REMOTE_ARCH)-$(REMOTE_OS)

.PHONY: remote-bootstrap
remote-bootstrap:
	$(MAKE) remote--send-src
	$(MAKE) remote--make-version
	$(MAKE) remote--make-dirs
	$(MAKE) remote--make-runtime
	$(MAKE) remote--recv-runtime
	$(MAKE) remote--gen-bootstrap-compiler-files
	$(MAKE) remote--send-bootstrap-compiler-files
	$(MAKE) remote--make-bootstrap-compiler
	$(MAKE) remote--make-script
	$(MAKE) remote--make-basis
	$(MAKE) remote--make-libraries
	$(MAKE) remote--send-mlyacc-yacc-files
	$(MAKE) remote--make-tools
	$(MAKE) remote--recv-boot-files
	$(MAKE) remote--make-clean
	$(MAKE) remote--send-boot-files
	$(MAKE) remote--make-all
	$(MAKE) remote--make-binary-release
	$(MAKE) remote--recv-binary-release
	$(MAKE) remote--rm-root

.PHONY: remote-add-target
remote-add-target:
	$(MAKE) remote--send-src
	$(MAKE) remote--make-version
	$(MAKE) remote--make-dirs
	$(MAKE) remote--make-runtime
	$(MAKE) remote--recv-runtime
ifneq ($(REMOTE_TARGET),$(CROSS_TARGET))
	$(MV) "$(LIB)/targets/$(REMOTE_TARGET)" "$(LIB)/targets/$(CROSS_TARGET)"
endif
	$(MAKE) remote--rm-root


.PHONY: remote--send-src
remote--send-src:
	$(SSH) $(REMOTE_MACHINE) "$(REMOTE_RM) $(REMOTE_ROOT) && $(REMOTE_MKDIR) $(REMOTE_ROOT)"
	$(GIT) archive --format=tar HEAD | $(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_TAR) xf -"

.PHONY: remote--send-mlyacc-yacc-files
remote--send-mlyacc-yacc-files:
	$(MAKE) -C mlyacc src/yacc.lex.sml src/yacc.grm.sig src/yacc.grm.sml
	$(TAR) cf - mlyacc/src/yacc.lex.* mlyacc/src/yacc.grm.* | $(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_TAR) xf -"
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && touch mlyacc/src/yacc.lex.* mlyacc/src/yacc.grm.*"

.PHONY: remote--recv-runtime
remote--recv-runtime:
	$(RM) "$(LIB)/targets/$(REMOTE_TARGET)" && $(MKDIR) "$(LIB)/targets/$(REMOTE_TARGET)"
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT)/build/lib/mlton/targets/self && $(REMOTE_TAR) cf - ." | (cd "$(LIB)/targets/$(REMOTE_TARGET)" && $(TAR) xf -)

.PHONY: remote-bootstrap-gen-bootstrap-compiler-files
remote--gen-bootstrap-compiler-files:
	$(MAKE) REMOTE_TARGET=$(REMOTE_TARGET) -C mlton mlton-bootstrap-$(REMOTE_TARGET).tgz

.PHONY: remote--send-bootstrap-compiler-files
remote--send-bootstrap-compiler-files:
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_RM) runtime/bootstrap && $(REMOTE_MKDIR) runtime/bootstrap"
	$(CAT) mlton/mlton-bootstrap-$(REMOTE_TARGET).tgz | $(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT)/runtime/bootstrap && $(REMOTE_TAR) xzf -"

.PHONY: remote--recv-boot-files
remote--recv-boot-files:
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_RM) boot && $(REMOTE_CP) build boot"
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_TAR) czf - boot" | $(CAT) - > "$(LIB)/targets/$(REMOTE_TARGET)/boot.tgz"

.PHONY: remote--send-boot-files
remote--send-boot-files:
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_RM) boot"
	$(CAT) "$(LIB)/targets/$(REMOTE_TARGET)/boot.tgz" | $(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_TAR) xzf -"

.PHONY: remote--recv-binary-release
remote--recv-binary-release:
	$(SSH) $(REMOTE_MACHINE) "cd $(REMOTE_ROOT) && $(REMOTE_CAT) mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(REMOTE_ARCH)-$(REMOTE_OS)$(MLTON_BINARY_RELEASE_SUFFIX).tgz" | $(CAT) - > mlton-$(MLTON_VERSION)-$(MLTON_BINARY_RELEASE).$(REMOTE_ARCH)-$(REMOTE_OS)$(MLTON_BINARY_RELEASE_SUFFIX).tgz

.PHONY: remote--rm-root
remote--rm-root:
	$(SSH) $(REMOTE_MACHINE) "$(REMOTE_RM) $(REMOTE_ROOT)"

define MK_REMOTE_MAKE_TEMPLATE
.PHONY: remote--make-$(1)
remote--make-$(1):
	$$(SSH) $$(REMOTE_MACHINE) "cd $$(REMOTE_ROOT) && $$(REMOTE_MAKE) $$(REMOTE_MAKEFLAGS) $$(REMOTE_XMAKEFLAGS) $$($(1)_REMOTE_XMAKEFLAGS) $(1)"
endef

$(eval $(call MK_REMOTE_MAKE_TEMPLATE,show-config))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,clean))
version_REMOTE_XMAKEFLAGS := MLTON_VERSION=$(MLTON_VERSION)
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,version))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,dirs))
runtime_REMOTE_XMAKEFLAGS := RELEASE=false
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,runtime))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,bootstrap-compiler))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,script))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,basis))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,libraries))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,check))
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,tools))
all_REMOTE_XMAKEFLAGS := OLD_MLTON_DIR=$(REMOTE_ROOT)/boot/bin
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,all))
binary-release_REMOTE_XMAKEFLAGS := MLTON_BINARY_RELEASE_WITH_DOCS=false
$(eval $(call MK_REMOTE_MAKE_TEMPLATE,binary-release))

endif

.PHONY: bootstrap-compiler
bootstrap-compiler:
	$(MAKE) -C "$(SRC)/runtime" "bootstrap/$(MLTON_OUTPUT)"
	$(CP) "$(SRC)/runtime/bootstrap/$(MLTON_OUTPUT)$(EXE)" "$(LIB)/"

######################################################################

# ?????

BSDSRC := /tmp/mlton-$(MLTON_VERSION)
MLTON_FREEBSD_RELEASE := 1
.PHONY: freebsd
freebsd:
	$(MAKE) clean clean-git version
	$(RM) "$(BSDSRC)"
	$(MKDIR) "$(BSDSRC)"
	( cd $(SRC) && $(TAR) -cpf - . ) | ( cd "$(BSDSRC)" && $(TAR) -xpf - )
	cd /tmp && $(TAR) -cpf - mlton-$(MLTON_VERSION) | \
		 $(GZIP) --force --best >/usr/ports/distfiles/mlton-$(MLTON_VERSION)-$(MLTON_FREEBSD_RELEASE).freebsd.src.tgz
        # do not change "make" to "$(MAKE)" in the following line
	cd "$(BSDSRC)/package/freebsd" && MAINTAINER_MODE=yes make build-package
