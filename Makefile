## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

export TARGET = self
export TARGET_ARCH = $(shell bin/host-arch)
export TARGET_OS = $(shell bin/host-os)
ROOT = $(shell pwd)
BUILD = $(ROOT)/build
SRC = $(ROOT)
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
INC = $(LIB)/include
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
AOUT = mlton-compile
ifeq (mingw, $(TARGET_OS))
EXE = .exe
else
EXE =
endif
MLBPATHMAP = $(LIB)/mlb-path-map
TARGETMAP = $(LIB)/target-map
SPEC = package/rpm/mlton.spec
LEX = mllex
PROF = mlprof
YACC = mlyacc
NLFFIGEN = mlnlffigen
PATH = $(BIN):$(SRC)/bin:$(shell echo $$PATH)
CP = /bin/cp -fpR
GZIP = gzip --force --best
RANLIB = ranlib

# If we're compiling with another version of MLton, then we want to do
# another round of compilation so that we get a MLton built without
# stubs.
ifeq (other, $(shell if [ ! -x $(BIN)/mlton ]; then echo other; fi))
	BOOTSTRAP_OTHER=true
else
	BOOTSTRAP_OTHER=false
endif

VERSION ?= $(shell date +%Y%m%d)
RELEASE ?= 1

.PHONY: all
all:
	$(MAKE) docs all-no-docs

.PHONY: all-no-docs
all-no-docs:
	$(MAKE) dirs runtime compiler world-no-check script mlbpathmap targetmap constants libraries tools
# Remove $(AOUT) so that the $(MAKE) compiler below will remake MLton.
# We also want to re-run the just-built tools (mllex and mlyacc)
# because they may be better than those that were used for the first
# round of compilation.  So, we clean out the front end.
ifeq (true, $(BOOTSTRAP_OTHER))
	rm -f $(COMP)/$(AOUT)$(EXE)
	$(MAKE) -C $(COMP)/front-end clean
endif
	$(MAKE) compiler world
	@echo 'Build of MLton succeeded.'

.PHONY: basis-no-check
basis-no-check:
	mkdir -p $(LIB)/sml
	rm -rf $(LIB)/sml/basis
	$(CP) $(SRC)/basis-library/. $(LIB)/sml/basis
	find $(LIB)/sml/basis -type d -name .svn | xargs rm -rf
	find $(LIB)/sml/basis -type f -name .ignore | xargs rm -rf

.PHONY: basis
basis:
	$(MAKE) basis-no-check
	@echo 'Type checking basis.'
	$(MLTON) -disable-ann deadCode \
		-stop tc \
		'$$(SML_LIB)/basis/libs/all.mlb' \
		>/dev/null

.PHONY: bootstrap-nj
bootstrap-nj:
	$(MAKE) nj-mlton
	$(MAKE) all

.PHONY: clean
clean:
	bin/clean

.PHONY: clean-svn
clean-svn:
	find . -type d | grep .svn | xargs rm -rf

.PHONY: compiler
compiler:
	$(MAKE) -C $(COMP)
	$(CP) $(COMP)/$(AOUT)$(EXE) $(LIB)/

.PHONY: constants
constants:
	@echo 'Creating constants file.'
	$(BIN)/mlton -build-constants true >tmp.c
	$(BIN)/mlton -output tmp tmp.c
	./tmp >$(LIB)/$(TARGET)/constants
	rm -f tmp tmp.c

DEBSRC = mlton-$(VERSION).orig
.PHONY: deb
deb:
	$(MAKE) clean clean-svn version
	mv package/debian .
	tar -cpf - . | \
		( cd .. && mkdir $(DEBSRC) && cd $(DEBSRC) && tar -xpf - )
	cd .. && tar -cpf - $(DEBSRC) | $(GZIP) >mlton_$(VERSION).orig.tar.gz
	cd .. && mv $(DEBSRC) mlton-$(VERSION)
	cd ../mlton-$(VERSION) && pdebuild --pbuilderroot ss

.PHONY: deb-binary
deb-binary:
	fakeroot debian/rules binary

.PHONY: deb-change
deb-change:
	(								\
		echo 'mlton ($(VERSION)-1) unstable; urgency=low';	\
		echo;							\
		echo '  * new upstream version';			\
		echo;							\
		echo ' -- Stephen Weeks <sweeks@sweeks.com>  '`date -R`;\
		echo;							\
		cat package/debian/changelog;				\
	) >/tmp/changelog
	mv /tmp/changelog package/debian/changelog

.PHONY: deb-lint
deb-lint:
	lintian ../mlton_$(VERSION)-1_i386.deb

.PHONY: deb-spell
deb-spell:
	ispell -g package/debian/control

.PHONY: dirs
dirs:
	mkdir -p $(BIN) $(LIB)/$(TARGET) $(INC)

.PHONY: docs
docs: dirs
	$(MAKE) -C $(LEX) docs
	$(MAKE) -C $(YACC) docs
	if htmldoc --version >/dev/null 2>&1; then \
		bin/make-pdf-guide; \
	fi

BSDSRC = /tmp/mlton-$(VERSION)
.PHONY: freebsd
freebsd:
	$(MAKE) clean clean-svn version
	rm -rf $(BSDSRC)
	mkdir -p $(BSDSRC)
	( cd $(SRC) && tar -cpf - . ) | ( cd $(BSDSRC) && tar -xpf - )
	cd /tmp && tar -cpf - mlton-$(VERSION) | \
		 $(GZIP) >/usr/ports/distfiles/mlton-$(VERSION)-$(RELEASE).freebsd.src.tgz
        # do not change "make" to "$(MAKE)" in the following line
	cd $(BSDSRC)/package/freebsd && MAINTAINER_MODE=yes make build-package  

LIBRARIES = ckit-lib cml mlnlffi-lib mlyacc-lib smlnj-lib

.PHONY: libraries-no-check
libraries-no-check:
	mkdir -p $(LIB)/sml
	cd $(LIB)/sml && rm -rf $(LIBRARIES)
	$(MAKE) -C $(SRC)/lib/ckit-lib
	$(MAKE) -C $(SRC)/lib/smlnj-lib
	$(CP) $(SRC)/lib/cml/. $(LIB)/sml/cml
	$(CP) $(SRC)/lib/ckit-lib/ckit/. $(LIB)/sml/ckit-lib
	$(CP) $(SRC)/lib/mlnlffi/. $(LIB)/sml/mlnlffi-lib
	$(CP) $(SRC)/lib/mlyacc/. $(LIB)/sml/mlyacc-lib
	$(CP) $(SRC)/lib/smlnj-lib/smlnj-lib/. $(LIB)/sml/smlnj-lib
	find $(LIB)/sml -type d -name .svn | xargs rm -rf
	find $(LIB)/sml -type f -name .ignore | xargs rm -rf

.PHONY: libraries
libraries:
	$(MAKE) libraries-no-check
	for f in $(LIBRARIES); do				\
		echo "Type checking $$f library.";		\
		$(MLTON) -disable-ann deadCode 			\
			-stop tc 				\
			'$$(SML_LIB)/'"$$f/$$f.mlb" 		\
			>/dev/null;				\
	done

.PHONY: nj-mlton
nj-mlton:
	$(MAKE) dirs runtime 
	$(MAKE) -C $(COMP) nj-mlton
	$(MAKE) script basis-no-check mlbpathmap targetmap constants libraries-no-check
	@echo 'Build of MLton succeeded.'

.PHONY: nj-mlton-dual
nj-mlton-dual:
	$(MAKE) dirs runtime
	$(MAKE) -C $(COMP) nj-mlton-dual
	$(MAKE) script basis-no-check mlbpathmap targetmap constants libraries-no-check
	@echo 'Build of MLton succeeded.'

.PHONY: nj-mlton-quad
nj-mlton-quad:
	$(MAKE) dirs runtime
	$(MAKE) -C $(COMP) nj-mlton-quad
	$(MAKE) script basis-no-check mlbpathmap targetmap constants libraries-no-check
	@echo 'Build of MLton succeeded.'

.PHONY: mlbpathmap
mlbpathmap:
	touch $(MLBPATHMAP)
	( echo 'MLTON_ROOT $$(LIB_MLTON_DIR)/sml';	\
	 echo 'SML_LIB $$(LIB_MLTON_DIR)/sml' ) 	\
		>>$(MLBPATHMAP).tmp
	mv $(MLBPATHMAP).tmp $(MLBPATHMAP)

.PHONY: traced
traced:
	$(MAKE) -C $(COMP) AOUT=$(AOUT).trace COMPILE_ARGS="-const 'Exn.keepHistory true' -const 'MLton.debug true' -drop-pass 'deepFlatten'"
	$(CP) $(COMP)/$(AOUT).trace $(LIB)/
	$(LIB)/$(AOUT).trace @MLton -- $(LIB)/world.trace
	sed 's/mlton-compile/mlton-compile.trace/' < $(MLTON) | sed 's/world.mlton/world.trace.mlton/' > $(MLTON).trace
	chmod a+x $(MLTON).trace

.PHONY: debugged
debugged:
	$(MAKE) -C $(COMP) AOUT=$(AOUT).debug COMPILE_ARGS="-debug true -const 'Exn.keepHistory true' -const 'MLton.debug true' -drop-pass 'deepFlatten'"
	$(CP) $(COMP)/$(AOUT).debug $(LIB)/
	$(LIB)/$(AOUT).debug @MLton -- $(LIB)/world.debug
	sed 's/mlton-compile/mlton-compile.debug/' < $(MLTON) | sed 's/world.mlton/world.debug.mlton/' > $(MLTON).debug
	chmod a+x $(MLTON).debug

.PHONY: profiled
profiled:
	$(MAKE) -C $(COMP) AOUT=$(AOUT).alloc COMPILE_ARGS="-profile alloc"
	$(CP) $(COMP)/$(AOUT).alloc $(LIB)/
	$(MAKE) -C $(COMP) AOUT=$(AOUT).count COMPILE_ARGS="-profile count"
	$(CP) $(COMP)/$(AOUT).count $(LIB)/
	$(MAKE) -C $(COMP) AOUT=$(AOUT).time COMPILE_ARGS="-profile time"
	$(CP) $(COMP)/$(AOUT).time $(LIB)/
	$(LIB)/$(AOUT).alloc @MLton -- $(LIB)/world.alloc
	$(LIB)/$(AOUT).count @MLton -- $(LIB)/world.count
	$(LIB)/$(AOUT).time @MLton -- $(LIB)/world.time
	sed 's/mlton-compile/mlton-compile.alloc/' < $(MLTON) | sed 's/world.mlton/world.alloc.mlton/' > $(MLTON).alloc
	sed 's/mlton-compile/mlton-compile.count/' < $(MLTON) | sed 's/world.mlton/world.count.mlton/' > $(MLTON).count
	sed 's/mlton-compile/mlton-compile.time/' < $(MLTON) | sed 's/world.mlton/world.time.mlton/' > $(MLTON).time
	chmod a+x $(MLTON).alloc
	chmod a+x $(MLTON).count
	chmod a+x $(MLTON).time

TOPDIR = 'TOPDIR-unset'
SOURCEDIR = $(TOPDIR)/SOURCES/mlton-$(VERSION)
.PHONY: rpms
rpms:
	$(MAKE) clean clean-svn version
	mkdir -p $(TOPDIR)
	cd $(TOPDIR) && mkdir -p BUILD RPMS/i386 SOURCES SPECS SRPMS
	rm -rf $(SOURCEDIR)
	mkdir -p $(SOURCEDIR)
	( cd $(SRC) && tar -cpf - . ) | ( cd $(SOURCEDIR) && tar -xpf - )
	$(CP) $(SOURCEDIR)/$(SPEC) $(TOPDIR)/SPECS/mlton.spec
	( cd $(TOPDIR)/SOURCES && tar -cpf - mlton-$(VERSION) )		\
		| $(GZIP) >$(SOURCEDIR).tgz
	rm -rf $(SOURCEDIR)
	rpm -ba --quiet --clean $(TOPDIR)/SPECS/mlton.spec

.PHONY: runtime
runtime:
	@echo 'Compiling MLton runtime system for $(TARGET).'
	$(MAKE) -C runtime
	$(CP) $(RUN)/*.a $(LIB)/$(TARGET)/
	$(CP) runtime/*.h include/*.h $(INC)/
	mkdir -p $(INC)/platform
	$(CP) bytecode/interpret.h $(INC)
	$(CP) runtime/platform/*.h $(INC)/platform
	$(MAKE) -C bytecode
	bytecode/print-opcodes >$(LIB)/opcodes
	ar r $(LIB)/$(TARGET)/libmlton.a bytecode/interpret.o 
	ar r $(LIB)/$(TARGET)/libmlton-gdb.a bytecode/interpret-gdb.o 
	for x in $(LIB)/$(TARGET)/*.a; do $(RANLIB) $$x; done

.PHONY: script
script:
	$(CP) bin/mlton-script $(MLTON)
	chmod a+x $(MLTON)
	$(CP) $(SRC)/bin/platform $(LIB)

.PHONY: targetmap
targetmap:
	touch $(TARGETMAP)
	( sed '/$(TARGET)/d' <$(TARGETMAP); 			\
		echo '$(TARGET) $(TARGET_ARCH) $(TARGET_OS)' ) 	\
		>>$(TARGETMAP).tmp
	mv $(TARGETMAP).tmp $(TARGETMAP)

.PHONY: tools
tools:
	$(MAKE) -C $(LEX)
	$(MAKE) -C $(NLFFIGEN)
	$(MAKE) -C $(PROF)
	$(MAKE) -C $(YACC)
	$(CP) $(LEX)/$(LEX)$(EXE) 		\
		$(NLFFIGEN)/$(NLFFIGEN)$(EXE)	\
		$(PROF)/$(PROF)$(EXE)		\
		$(YACC)/$(YACC)$(EXE)		\
		$(BIN)/

.PHONY: version
version:
	@echo 'Instantiating version numbers.'
	for f in							\
		package/debian/changelog				\
		$(SPEC)							\
		package/freebsd/Makefile				\
		mlton/control/control-flags.sml;			\
	do								\
		sed "s/\(.*\)MLTONVERSION\(.*\)/\1$(VERSION)\2/" <$$f >z && \
		mv z $$f;						\
	done
	sed <$(SPEC) >z "/^Release:/s;.*;Release: $(RELEASE);"
	mv z $(SPEC)

.PHONY: world-no-check
world-no-check: 
	@echo 'Making world.'
	$(MAKE) basis-no-check
	$(LIB)/$(AOUT)$(EXE) @MLton -- $(LIB)/world

.PHONY: world
world: 
	$(MAKE) world-no-check
	@echo 'Type checking basis.'
	$(MLTON) -disable-ann deadCode \
		-stop tc \
		'$$(SML_LIB)/basis/libs/all.mlb' \
		>/dev/null

# The TBIN and TLIB are where the files are going to be after installing.
# The DESTDIR and is added onto them to indicate where the Makefile actually
# puts them.
DESTDIR = $(CURDIR)/install
PREFIX = /usr
ifeq ($(TARGET_OS), cygwin)
PREFIX = /
endif
ifeq ($(TARGET_OS), darwin)
PREFIX = /usr/local
endif
ifeq ($(TARGET_OS), mingw)
PREFIX = /mingw
endif
ifeq ($(TARGET_OS), solaris)
PREFIX = /usr/local
endif
prefix = $(PREFIX)
MAN_PREFIX_EXTRA =
TBIN = $(DESTDIR)$(prefix)/bin
ULIB = lib/mlton
TLIB = $(DESTDIR)$(prefix)/$(ULIB)
TMAN = $(DESTDIR)$(prefix)$(MAN_PREFIX_EXTRA)/man/man1
TDOC = $(DESTDIR)$(prefix)/share/doc/mlton
ifeq ($(TARGET_OS), cygwin)
TDOC = $(DESTDIR)$(prefix)/usr/share/doc/mlton
endif
ifeq ($(TARGET_OS), solaris)
TDOC = $(DESTDIR)$(prefix)/doc/mlton
endif
TEXM = $(TDOC)/examples

GZIP_MAN = true
ifeq ($(TARGET_OS), solaris)
GZIP_MAN = false
endif

.PHONY: install
install: install-docs install-no-docs

MAN_PAGES =  \
	mllex.1 \
	mlnlffigen.1 \
	mlprof.1 \
	mlton.1 \
	mlyacc.1

.PHONY: install-no-docs
install-no-docs:
	mkdir -p $(TLIB) $(TBIN) $(TMAN)
	$(CP) $(LIB)/. $(TLIB)/
	rm -f $(TLIB)/self/libmlton-gdb.a
	sed "/^lib=/s;.*;lib='$(prefix)/$(ULIB)';" 			\
		<$(SRC)/bin/mlton-script >$(TBIN)/mlton
	chmod a+x $(TBIN)/mlton
	cd $(BIN) && $(CP) $(LEX)$(EXE) $(NLFFIGEN)$(EXE)		\
		 $(PROF)$(EXE) $(YACC)$(EXE) $(TBIN)/
	( cd $(SRC)/man && tar cf - $(MAN_PAGES)) | \
		( cd $(TMAN)/ && tar xf - )
	if $(GZIP_MAN); then						\
		cd $(TMAN) && $(GZIP) $(MAN_PAGES);			\
	fi
	case "$(TARGET_OS)" in						\
	cygwin|darwin|solaris)						\
	;;								\
	*)								\
		for f in $(TLIB)/$(AOUT)$(EXE) $(TBIN)/$(LEX)$(EXE)	\
			$(TBIN)/$(NLFFIGEN)$(EXE) $(TBIN)/$(PROF)$(EXE)	\
			$(TBIN)/$(YACC)$(EXE); do			\
			strip --remove-section=.comment			\
				--remove-section=.note $$f; 		\
		done							\
	esac

.PHONY: install-docs
install-docs:
	mkdir -p $(TDOC)
	(								\
		cd $(SRC)/doc &&					\
		$(CP) changelog examples guide license README $(TDOC)/	\
	)
	mv $(TDOC)/guide/mlton-guide.pdf $(TDOC)/
	(								\
		cd $(SRC)/util &&					\
		$(CP) cmcat cm2mlb $(TDOC)/				\
	)
	for f in callcc command-line hello-world same-fringe signals	\
			size taut thread1 thread2 thread-switch timeout \
		; do 							\
		$(CP) $(SRC)/regression/$$f.sml $(TEXM)/; 		\
	done
	$(GZIP) -c $(LEX)/$(LEX).ps >$(TDOC)/$(LEX).ps.gz
	$(GZIP) -c $(YACC)/$(YACC).ps >$(TDOC)/$(YACC).ps.gz
	find $(TDOC)/ -name .svn -type d | xargs rm -rf
	find $(TDOC)/ -name .ignore -type f | xargs rm -rf
	find $(TEXM)/ -name .svn -type d | xargs rm -rf
	find $(TEXM)/ -name .ignore -type f | xargs rm -rf

TDOCBASE = $(DESTDIR)$(prefix)/share/doc-base

.PHONY: post-install-debian
post-install-debian:	
	cd $(TDOC)/ && rm -rf license
	$(CP) $(SRC)/debian/copyright $(SRC)/debian/README.Debian $(TDOC)/
	$(CP) $(SRC)/debian/changelog $(TDOC)/changelog.Debian
	mkdir -p $(TDOCBASE)
	for f in mllex mlton mlyacc; do \
		$(CP) $(SRC)/debian/$$f.doc-base $(TDOCBASE)/$$f; \
	done
	cd $(TDOC)/ && $(GZIP) changelog changelog.Debian
	chown -R root.root $(TDOC) $(TLIB)
