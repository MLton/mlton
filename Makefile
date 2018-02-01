## Copyright (C) 2009,2011,2013,2017-2018 Matthew Fluet.
 # Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

# Specify C compiler and binutils.
# Can be used for alternative tools (e.g., `CC=clang` or `CC=gcc-7`).
CC := gcc
AR := ar
RANLIB := ranlib

# Specify GMP include and library paths, if not on default search paths.
WITH_GMP_DIR :=
ifneq ($(WITH_GMP_DIR),)
WITH_GMP_INC_DIR := $WITH_GMP_DIR/include
WITH_GMP_LIB_DIR := $WITH_GMP_DIR/lib
endif

# Specify runtime and compile arguments given to (the to-be-built) `mlton` when
# compiling distributed executables ((self-compiled) `mlton`, `mllex`, `mlyacc`,
# `mlprof`, and `mlnlffigen`).
# Can be used for testing (e.g., `MLTON_COMPILE_ARGS="-codegen c"`) or for
# downstream packaging.
MLTON_RUNTIME_ARGS :=
MLTON_COMPILE_ARGS :=

# Specify runtime and compile arguments given to "old" `mlton` when compiling
# "bootstrapped" `mlton`.
# Can be used to work around bugs in "old" `mlton` when compiling "bootstrapped"
# `mlton` (e.g., `BOOTSTRAP_MLTON_COMPILE_ARGS="-drop-pass 'deepFlatten'"`).
BOOTSTRAP_MLTON_RUNTIME_ARGS :=
BOOTSTRAP_MLTON_COMPILE_ARGS :=

# Specify standard tools.
# Can be used for alternative tools (e.g., `SED=gsed`).
DIFF := diff
FIND := find
GIT := git
GREP := grep
GZIP := gzip
PATCH := patch
SED := sed
TAR := tar

######################################################################
######################################################################

SRC := $(shell pwd)
BUILD := $(SRC)/build
BIN := $(BUILD)/bin
LIB := $(BUILD)/lib/mlton
INC := $(LIB)/include

PATH := $(BIN):$(shell echo $$PATH)

VERSION := $(shell TZ=UTC $(GIT) log -n1 --date=format-local:"%Y%m%d.%H%M%S" --pretty=format:"%cd-g%h$$([ "$$($(GIT) status --porcelain 2> /dev/null)" ] && echo '-dirty')" 2> /dev/null || echo '????????')
RELEASE := 1

HOST_ARCH := $(shell ./bin/host-arch)
HOST_OS := $(shell ./bin/host-os)
TARGET := self
TARGET_ARCH := $(HOST_ARCH)
TARGET_OS := $(HOST_OS)

ifeq (mingw, $(TARGET_OS))
EXE := .exe
else
EXE :=
endif

CP := cp -fpR
MKDIR := mkdir -p
MV := mv -f
RM := rm -rf

######################################################################

# If we're compiling with another version of MLton, then we want to do another
# round of compilation so that we get a MLton built without stubs.
ifeq (other, $(shell if [ ! -x "$(BIN)/mlton" ]; then echo other; fi))
BOOTSTRAP:=true
else
BOOTSTRAP:=false
endif
CHECK_FIXPOINT:=false

.PHONY: all
all:
	$(MAKE) docs all-no-docs

.PHONY: all-no-docs
all-no-docs:
	$(MAKE) dirs runtime
	$(MAKE) compiler CHECK_FIXPOINT=false  # tools0 + mlton0 -> mlton1
	$(MAKE) script mlbpathmap basis-no-check constants basis-check libraries
	$(MAKE) tools    CHECK_FIXPOINT=false  # tools0 + mlton1 -> tools1
ifeq (true, $(findstring true,$(BOOTSTRAP) $(CHECK_FIXPOINT)))
	$(RM) "$(SRC)/mlton/mlton-compile$(EXE)"
	$(MAKE) -C "$(SRC)/mlton/front-end" clean
	$(MAKE) compiler CHECK_FIXPOINT=false  # tools1 + mlton1 -> mlton2
ifeq (true, $(CHECK_FIXPOINT))
	$(MAKE) tools-clean
	$(MAKE) tools    CHECK_FIXPOINT=true   # tools1 + mlton1 -> tools2; tools2 == tools1
	$(RM) "$(SRC)/mlton/mlton-compile$(EXE)"
	$(MAKE) -C "$(SRC)/mlton/front-end" clean
	$(MAKE) compiler CHECK_FIXPOINT=true   # tools2 + mlton2 -> mlton3; mlton3 == mlton2
endif
endif
	@echo 'Build of MLton succeeded.'

.PHONY: basis-no-check
basis-no-check:
	$(MKDIR) "$(LIB)/sml"
	$(RM) "$(LIB)/sml/basis"
	$(CP) "$(SRC)/basis-library/." "$(LIB)/sml/basis"
	$(FIND) "$(LIB)/sml/basis" -name .gitignore -exec $(RM) '{}' ';'

.PHONY: basis-check
basis-check:
	@echo 'Type checking basis.'
	"$(BIN)/mlton" -disable-ann deadCode \
		-stop tc \
		'$$(SML_LIB)/basis/libs/all.mlb' \
		>/dev/null

.PHONY: basis
basis:
	$(MAKE) basis-no-check
	$(MAKE) basis-check

.PHONY: bootstrap-smlnj
bootstrap-smlnj:
	$(MAKE) smlnj-mlton
	$(RM) "$(BIN)/mlton"
	$(MAKE) BOOTSTRAP_MLTON=mlton.smlnj all

.PHONY: bootstrap-polyml
bootstrap-polyml:
	$(MAKE) polyml-mlton
	$(RM) "$(BIN)/mlton"
	$(MAKE) BOOTSTRAP_MLTON=mlton.polyml all

.PHONY: clean
clean:
	./bin/clean

.PHONY: clean-git
clean-git:
	$(FIND) . -type d -name .git -prune -exec $(RM) '{}' ';'

.PHONY: compiler
compiler:
	$(MAKE) -C "$(SRC)/mlton" MLTON_OUTPUT=mlton-compile
ifeq (true, $(CHECK_FIXPOINT))
	$(DIFF) -b "$(SRC)/mlton/mlton-compile$(EXE)" "$(LIB)/mlton-compile$(EXE)"
endif
	$(CP) "$(SRC)/mlton/mlton-compile$(EXE)" "$(LIB)/"

.PHONY: constants
constants:
	@echo 'Creating constants file.'
	"$(BIN)/mlton" -target "$(TARGET)" -build-constants true > build-constants.c
	"$(BIN)/mlton" -target "$(TARGET)" -output build-constants build-constants.c
	./build-constants$(EXE) >"$(LIB)/targets/$(TARGET)/constants"
	$(RM) build-constants$(EXE) build-constants.c

.PHONY: debugged
debugged:
	$(MAKE) -C "$(SRC)/mlton" MLTON_OUTPUT=mlton-compile.debug \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -debug true -const 'Exn.keepHistory true' -profile-val true -const 'MLton.debug true' -disable-pass 'deepFlatten'"
	$(CP) "$(SRC)/mlton/mlton-compile.debug$(EXE)" "$(LIB)/"
	$(SED) -e 's/mlton-compile/mlton-compile.debug/' \
		< "$(BIN)/mlton" \
		> "$(BIN)/mlton.debug"
	chmod u+x "$(BIN)/mlton.debug"

.PHONY: dirs
dirs:
	$(MKDIR) "$(BIN)" "$(LIB)" "$(INC)"
	$(MKDIR) "$(LIB)/targets/$(TARGET)/include"
	$(MKDIR) "$(LIB)/targets/$(TARGET)/sml"

.PHONY: docs
docs: dirs
	$(MAKE) -C "$(SRC)/mllex" docs
	$(MAKE) -C "$(SRC)/mlyacc" docs
	$(MAKE) -C "$(SRC)/doc/guide"

LIBRARIES := ckit-lib cml mllpt-lib mlnlffi-lib mlrisc-lib mlyacc-lib smlnj-lib

.PHONY: libraries-no-check
libraries-no-check:
	$(MKDIR) "$(LIB)/sml"
	cd "$(LIB)/sml" && $(RM) $(LIBRARIES)
	$(MAKE) -C "$(SRC)/lib/ckit-lib"
	$(CP) "$(SRC)/lib/ckit-lib/ckit/." "$(LIB)/sml/ckit-lib"
	$(CP) "$(SRC)/lib/cml/." "$(LIB)/sml/cml"
	$(MAKE) -C "$(SRC)/lib/mllpt-lib"
	$(CP) "$(SRC)/lib/mllpt-lib/ml-lpt/lib/." "$(LIB)/sml/mllpt-lib"
	$(MAKE) -C "$(SRC)/lib/mlnlffi-lib"
	$(CP) "$(SRC)/lib/mlnlffi-lib/." "$(LIB)/sml/mlnlffi-lib"
	$(MAKE) -C "$(SRC)/lib/mlrisc-lib"
	$(CP) "$(SRC)/lib/mlrisc-lib/MLRISC/." "$(LIB)/sml/mlrisc-lib"
	$(CP) "$(SRC)/lib/mlyacc-lib/." "$(LIB)/sml/mlyacc-lib"
	$(MAKE) -C "$(SRC)/lib/smlnj-lib"
	$(CP) "$(SRC)/lib/smlnj-lib/smlnj-lib/." "$(LIB)/sml/smlnj-lib"
	$(FIND) "$(LIB)/sml" -type d -name .cm -prune -exec $(RM) '{}' ';'
	$(FIND) "$(LIB)/sml" -name .gitignore -exec $(RM) '{}' ';'

define LIBRARIES_CHECK_TEMPLATE
	@echo "Type checking $(1) library."
	"$(BIN)/mlton" -disable-ann deadCode		\
		-stop tc				\
		'$$(SML_LIB)/$(1)/$(1).mlb'		\
		>/dev/null
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

.PHONY: mlbpathmap
mlbpathmap:
	( echo 'MLTON_ROOT $$(LIB_MLTON_DIR)/sml';	\
	  echo 'SML_LIB $$(LIB_MLTON_DIR)/sml'; )	\
		> "$(LIB)/mlb-path-map"

.PHONY: polyml-mlton
polyml-mlton:
	$(MAKE) dirs runtime
	$(MAKE) -C "$(SRC)/mlton" polyml-mlton
	$(CP) "$(SRC)/mlton/mlton-polyml$(EXE)" "$(LIB)/"
	$(MAKE) script mlbpathmap basis-no-check constants libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitSMLNJ "$$@";# doitSMLNJ "$$@";' \
		< "$(BIN)/mlton" \
		> "$(BIN)/mlton.polyml"
	chmod u+x "$(BIN)/mlton.polyml"
	@echo 'Build of MLton (with Poly/ML) succeeded.'

define PROFILED_TEMPLATE
	$(MAKE) -C "$(SRC)/mlton" MLTON_OUTPUT=mlton-compile.$(1) \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -profile $(1)"
	$(CP) "$(SRC)/mlton/mlton-compile.$(1)$(EXE)" "$(LIB)/"
	$(SED) -e "s/mlton-compile/mlton-compile.$(1)/" \
		< "$(BIN)/mlton" \
		>"$(BIN)/mlton.$(1)"
	chmod u+x "$(BIN)/mlton.$(1)"
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
	$(MAKE) profiled-alloc
	$(MAKE) profiled-count
	$(MAKE) profiled-time

.PHONY: runtime
runtime:
	@echo 'Compiling MLton runtime system for $(TARGET).'
	$(MAKE) -C "$(SRC)/runtime"
	$(CP) "$(SRC)/include/"*.h "$(INC)/"
	$(CP) "$(SRC)/runtime/"*.a "$(LIB)/targets/$(TARGET)/"
	$(CP) "$(SRC)/runtime/gen/sizes" "$(LIB)/targets/$(TARGET)/"
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
		-e "s;^CC=.*;CC=\"$(CC)\";" \
		-e "s;^GMP_INC_DIR=.*;GMP_INC_DIR=\"$(WITH_GMP_INC_DIR)\";" \
		-e "s;^GMP_LIB_DIR=.*;GMP_LIB_DIR=\"$(WITH_GMP_LIB_DIR)\";" \
		< "$(SRC)/bin/mlton-script" > "$(BIN)/mlton"
	chmod a+x "$(BIN)/mlton"
	$(CP) "$(SRC)/bin/platform" "$(LIB)"
	$(CP) "$(SRC)/bin/static-library" "$(LIB)"
ifeq (mingw, $(TARGET_OS))
	$(CP) "$(SRC)/bin/static-library.bat" "$(LIB)"
endif

.PHONY: smlnj-mlton
smlnj-mlton:
	$(MAKE) dirs runtime
	$(MAKE) -C "$(SRC)/mlton" smlnj-mlton
	smlnj_heap_suffix=`echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null` && $(CP) "$(SRC)/mlton/mlton-smlnj.$$smlnj_heap_suffix" "$(LIB)/"
	$(MAKE) script mlbpathmap basis-no-check constants libraries-no-check
	$(SED) \
		-e 's;doitMLton "$$@";# doitMLton "$$@";' \
		-e 's;doitPolyML "$$@";# doitPolyML "$$@";' \
		< "$(BIN)/mlton" \
		> "$(BIN)/mlton.smlnj"
	chmod u+x "$(BIN)/mlton.smlnj"
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

.PHONY: traced
traced:
	$(MAKE) -C "$(SRC)/mlton" MLTON_OUTPUT=mlton-compile.trace \
		MLTON_COMPILE_ARGS="$(MLTON_COMPILE_ARGS) -const 'Exn.keepHistory true' -profile-val true -const 'MLton.debug true' -disable-pass 'deepFlatten'"
	$(CP) "$(SRC)/mlton/mlton-compile.trace$(EXE)" "$(LIB)/"
	$(SED) -e 's/mlton-compile/mlton-compile.trace/' \
		< "$(BIN)/mlton" \
		> "$(BIN)/mlton.trace"
	chmod u+x "$(BIN)/mlton.trace"

ifeq (true, $(CHECK_FIXPOINT))
define TOOLS_TEMPLATE_CHECK_FIXPOINT
	$(DIFF) -b "$(SRC)/$(1)/$(1)$(EXE)" "$(BIN)/$(1)$(EXE)"
endef
else
define TOOLS_TEMPLATE_CHECK_FIXPOINT
endef
endif

define TOOLS_TEMPLATE =
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

.PHONY: version
version:
	@echo 'Instantiating version numbers.'
	for f in							\
		package/freebsd/Makefile				\
		mlton/control/version_sml.src				\
		doc/guide/conf/asciidoc-mlton.flags			\
	; do								\
		if grep -q 'MLTONVERSION' "$$f"; then			\
			$(SED) -e "s/\(.*\)MLTONVERSION\(.*\)/\1$(VERSION)\2/" <"$$f" >z && 	\
			mv z "$$f";							\
		fi;							\
	done

.PHONY: vars
vars:
	@echo VERSION = "$(VERSION)"
	@echo RELEASE = "$(RELEASE)"

.PHONY: check
check:
	./bin/regression $(CHECK_ARGS)

# The TBIN and TLIB are where the files are going to be after installing.
# The DESTDIR is added onto them to indicate where the Makefile actually
# puts them.
PREFIX := /usr
MAN_PREFIX_EXTRA :=
TBIN := $(PREFIX)/bin
ULIB := lib/mlton
TLIB := $(PREFIX)/$(ULIB)
TMAN := $(PREFIX)$(MAN_PREFIX_EXTRA)/man/man1
TDOC := $(PREFIX)/share/doc/mlton
ifeq ($(findstring $(TARGET_OS), solaris mingw), $(TARGET_OS))
TDOC := $(PREFIX)/doc/mlton
endif
TEXM := $(TDOC)/examples

GZIP_MAN := true
ifeq ($(findstring $(TARGET_OS), openbsd solaris), $(TARGET_OS))
GZIP_MAN := false
endif

.PHONY: install
install: install-no-strip install-strip

.PHONY: install-no-strip
install-no-strip: install-docs install-no-docs move-docs

MAN_PAGES :=  \
	mllex.1 \
	mlnlffigen.1 \
	mlprof.1 \
	mlton.1 \
	mlyacc.1

.PHONY: install-no-docs
install-no-docs:
	$(MKDIR) "$(TLIB)" "$(TBIN)" "$(TMAN)"
	$(CP) "$(LIB)/." "$(TLIB)/"
	$(CP) "$(BIN)/." "$(TBIN)/"
	for f in $(MAN_PAGES); do					\
		$(CP) "$(SRC)/man/$$f" "$(TMAN)"			\
	done
	if $(GZIP_MAN); then						\
		cd "$(TMAN)" && $(GZIP) --force --best $(MAN_PAGES);	\
	fi

.PHONY: install-strip
install-strip:
	case "$(TARGET_OS)" in						\
	aix|cygwin|darwin|solaris)					\
	;;								\
	*)								\
		for f in "$(TLIB)/mlton-compile$(EXE)" 			\
			"$(TBIN)/mllex$(EXE)"				\
			"$(TBIN)/mlyacc$(EXE)"				\
			"$(TBIN)/mlprof$(EXE)" 				\
			"$(TBIN)/mlnlffigen$(EXE)"; do			\
			strip --remove-section=.comment			\
				--remove-section=.note "$$f";		\
		done							\
	esac

.PHONY: install-docs
install-docs:
	$(MKDIR) "$(TDOC)"
	(								\
		cd "$(SRC)/doc" &&					\
		$(CP) changelog examples license README "$(TDOC)/"	\
	)
	if [ -d "$(SRC)/doc/guide/localhost" ]; then			\
		$(CP) "$(SRC)/doc/guide/localhost" "$(TDOC)/guide";	\
	fi
	if [ -r "$(SRC)/doc/guide/mlton-guide.pdf" ]; then		\
		$(CP) "$(SRC)/doc/guide/mlton-guide.pdf" "$(TDOC)/";	\
	fi
	(								\
		cd "$(SRC)/util" &&					\
		$(CP) cmcat cm2mlb "$(TDOC)/"				\
	)
	for f in callcc command-line hello-world same-fringe signals	\
			size taut thread1 thread2 thread-switch timeout \
		; do							\
		$(CP) "$(SRC)/regression/$$f.sml" "$(TEXM)/";		\
	done
	if [ -r "mllex/mllex.pdf" ]; then				\
		$(CP) "mllex/mllex.pdf" "$(TDOC)/";			\
	fi
	if [ -r "mlyacc/mlyacc.pdf" ]; then				\
		$(CP) "mlyacc/mlyacc.pdf" "$(TDOC)/";			\
	fi
	$(FIND) "$(TDOC)/" -name .gitignore -exec $(RM) '{}' ';'
	$(FIND) "$(TEXM)/" -name .gitignore -exec $(RM) '{}' ';'


.PHONY: move-docs
move-docs: install-docs install-no-docs
	cd "$(TLIB)/sml"; for i in *; do test -d "$(TDOC)/$$i" || $(MKDIR) "$(TDOC)/$$i"; done
	cd "$(TLIB)/sml"; for i in */[Dd]oc; do $(MV) "$$i" "$(TDOC)/$$i"; done
	cd "$(TLIB)/sml"; for i in */README*; do $(MV) "$$i" "$(TDOC)/$$i"; done

.PHONY: release
release: version
	$(TAR) cvzf ../mlton-$(VERSION).src.tgz \
		--exclude .git --exclude package \
		--transform "s@^@mlton-$(VERSION)/@S" \
		*
	$(MAKE) clean

.PHONY: dist
dist: release

.PHONY: binary-release
binary-release:
	$(MKDIR) mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS)
	$(MAKE) PREFIX=mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS) install
	$(TAR) cvzf ../mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS).tgz \
		mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS)
	$(RM) mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS)

BSDSRC := /tmp/mlton-$(VERSION)
.PHONY: freebsd
freebsd:
	$(MAKE) clean clean-git version
	$(RM) "$(BSDSRC)"
	$(MKDIR) "$(BSDSRC)"
	( cd $(SRC) && tar -cpf - . ) | ( cd "$(BSDSRC)" && tar -xpf - )
	cd /tmp && tar -cpf - mlton-$(VERSION) | \
		 $(GZIP) --force --best >/usr/ports/distfiles/mlton-$(VERSION)-$(RELEASE).freebsd.src.tgz
        # do not change "make" to "$(MAKE)" in the following line
	cd "$(BSDSRC)/package/freebsd" && MAINTAINER_MODE=yes make build-package
