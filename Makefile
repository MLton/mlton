export TARGET = self
export TARGET_ARCH = $(shell bin/host-arch)
export TARGET_OS = $(shell bin/host-os)
ROOT = $(shell pwd)
BUILD = $(ROOT)/build
SRC = $(ROOT)
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
AOUT = mlton-compile
TARGETMAP = $(LIB)/target-map
SPEC = $(SRC)/doc/mlton.spec
LEX = mllex
PROF = mlprof
YACC = mlyacc
PATH = $(BIN):$(shell echo $$PATH)
CP = /bin/cp -fpR
GZIP = gzip --force --best

VERSION = $(shell date +%Y%m%d)
RELEASE = 1

.PHONY: all
all:
	$(MAKE) dirs docs runtime compiler world-no-check
# If we're compiling with another version of MLton, then we want to do
# another round of compilation so that we get a MLton built without
# stubs.  Remove $(AOUT) so that the $(MAKE) compiler below will
# remake MLton.
ifeq (other, $(shell if [ ! -x $(BIN)/mlton ]; then echo other; fi))
	rm -f $(COMP)/$(AOUT)
endif
	$(MAKE) script targetmap constants compiler world tools
	@echo 'Build of MLton succeeded.'

.PHONY: bootstrap-nj
bootstrap-nj:
	$(MAKE) nj-mlton
	$(MAKE) all

.PHONY: clean
clean:
	bin/clean

.PHONY: clean-cvs
clean-cvs:
	find . -type d | grep CVS | xargs rm -rf

.PHONY: cm
cm:
	$(MAKE) -C $(COMP) mlton-stubs_cm
	$(MAKE) -C $(LEX) mllex_cm
	$(MAKE) -C $(PROF) mlprof_cm
	$(MAKE) -C $(YACC) mlyacc_cm
	$(MAKE) -C benchmark benchmark_cm

.PHONY: compiler
compiler:
	$(MAKE) -C $(COMP)
	$(CP) $(COMP)/$(AOUT) $(LIB)/

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
	$(MAKE) clean clean-cvs version
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
		cat debian/changelog;					\
	) >/tmp/changelog
	mv /tmp/changelog debian/changelog

.PHONY: deb-lint
deb-lint:
	lintian ../mlton_$(VERSION)-1_i386.deb

.PHONY: deb-spell
deb-spell:
	ispell -g debian/control

.PHONY: dirs
dirs:
	mkdir -p $(BIN) $(LIB)/$(TARGET) $(LIB)/include

.PHONY: docs
docs:
	$(MAKE) -C $(SRC)/doc/user-guide
	$(MAKE) -C $(LEX) docs
	$(MAKE) -C $(YACC) docs

BSDSRC = /tmp/mlton-$(VERSION)
.PHONY: freebsd
freebsd:
	$(MAKE) clean clean-cvs version
	rm -rf $(BSDSRC)
	mkdir -p $(BSDSRC)
	( cd $(SRC) && tar -cpf - . ) | ( cd $(BSDSRC) && tar -xpf - )
	cd /tmp && tar -cpf - mlton-$(VERSION) | \
		 $(GZIP) >/usr/ports/distfiles/mlton-$(VERSION)-1.src.tgz
	cd $(BSDSRC)/freebsd && make build-package

#	rm -rf $(BSDSRC)

.PHONY: targetmap
targetmap:
	touch $(TARGETMAP)
	( sed '/$(TARGET)/d' <$(TARGETMAP); 			\
		echo '$(TARGET) $(TARGET_ARCH) $(TARGET_OS)' ) 	\
		>>$(TARGETMAP).tmp
	mv $(TARGETMAP).tmp $(TARGETMAP)

.PHONY: nj-mlton
nj-mlton:
	$(MAKE) dirs
	$(MAKE) -C $(COMP) nj-mlton
	$(MAKE) script runtime targetmap constants
	@echo 'Build of MLton succeeded.'

.PHONY: nj-mlton-dual
nj-mlton-dual:
	$(MAKE) dirs	
	$(MAKE) -C $(COMP) nj-mlton-dual
	$(MAKE) script runtime targetmap constants
	@echo 'Build of MLton succeeded.'

TOPDIR = 'TOPDIR-unset'
SOURCEDIR = $(TOPDIR)/SOURCES/mlton-$(VERSION)
.PHONY: rpms
rpms:
	$(MAKE) clean clean-cvs version
	mkdir -p $(TOPDIR)
	cd $(TOPDIR) && mkdir -p BUILD RPMS/i386 SOURCES SPECS SRPMS
	rm -rf $(SOURCEDIR)
	mkdir -p $(SOURCEDIR)
	( cd $(SRC) && tar -cpf - . ) | ( cd $(SOURCEDIR) && tar -xpf - )
	$(CP) $(SOURCEDIR)/doc/mlton.spec $(TOPDIR)/SPECS/mlton.spec
	( cd $(TOPDIR)/SOURCES && tar -cpf - mlton-$(VERSION) )		\
		| $(GZIP) >$(SOURCEDIR).tgz
	rm -rf $(SOURCEDIR)
	rpm -ba --quiet --clean $(TOPDIR)/SPECS/mlton.spec

.PHONY: runtime
runtime:
	@echo 'Compiling MLton runtime system for $(TARGET).'
	$(MAKE) -C runtime
	$(CP) $(RUN)/*.a $(LIB)/$(TARGET)/
	$(CP) runtime/*.h include/*.h $(LIB)/include/

.PHONY: script
script:
	@echo 'Setting lib in mlton script.'
	sed "/^lib=/s;'.*';\"\`dirname \$$0\`/../lib\";" <bin/mlton >$(MLTON)
	chmod a+x $(MLTON)
	$(CP) $(SRC)/bin/platform $(LIB)

.PHONY: tools
tools:
	$(MAKE) -C $(LEX)
	$(MAKE) -C $(PROF)
	$(MAKE) -C $(YACC)
	$(CP) $(LEX)/$(LEX) $(PROF)/$(PROF) $(YACC)/$(YACC) $(BIN)/

.PHONY: version
version:
	@echo 'Instantiating version numbers.'
	for f in							\
		debian/changelog					\
		doc/mlton.spec						\
		doc/user-guide/macros.tex				\
		freebsd/Makefile					\
		mlton/control/control.sml; 				\
	do								\
		sed "s/\(.*\)MLTONVERSION\(.*\)/\1$(VERSION)\2/" <$$f >z && \
		mv z $$f;						\
	done
	sed <$(SPEC) >z "/^Release:/s;.*;Release: $(RELEASE);"
	mv z $(SPEC)

.PHONY: world
world: 
	$(MAKE) world-no-check
	@echo 'Type checking basis.'
	$(MLTON) -dead-code false	\
		-sequence-unit true 	\
		-stop tc 		\
		-warn-unused true	\
		>/dev/null

.PHONY: world-no-check
world-no-check: 
	@echo 'Making world.'
	rm -rf $(LIB)/sml
	mkdir $(LIB)/sml
	$(CP) $(SRC)/basis-library $(LIB)/sml
	find $(LIB)/sml -type f -name .cvsignore | xargs rm -rf
	$(LIB)/$(AOUT) @MLton -- $(LIB)/world

# The TBIN and TLIB are where the files are going to be after installing.
# The DESTDIR and is added onto them to indicate where the Makefile actually
# puts them.
DESTDIR = $(CURDIR)/install
PREFIX = /usr
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

.PHONY: install-no-docs
install-no-docs:
	mkdir -p $(TLIB) $(TBIN) $(TMAN)
	$(CP) $(LIB)/. $(TLIB)/
	sed "/^lib=/s;'.*';'$(prefix)/$(ULIB)';" 			\
			<$(SRC)/bin/mlton >$(TBIN)/mlton
	chmod +x $(TBIN)/mlton
	$(CP) $(SRC)/bin/platform $(LIB)
	$(CP) $(BIN)/$(LEX) $(BIN)/$(PROF) $(BIN)/$(YACC) $(TBIN)/
	( cd $(SRC)/man && tar cf - mllex.1 mlprof.1 mlton.1 mlyacc.1 ) | \
		( cd $(TMAN)/ && tar xf - )
	if $(GZIP_MAN); then						\
		cd $(TMAN) && $(GZIP) mllex.1 mlprof.1 mlton.1		\
			mlyacc.1;					\
	fi
	if [ $(TARGET_OS) != solaris ]; then					\
	for f in $(TLIB)/$(AOUT) 						\
		$(TBIN)/$(LEX) $(TBIN)/$(PROF) $(TBIN)/$(YACC); do 		\
		strip --remove-section=.comment --remove-section=.note $$f; 	\
	done									\
	fi

.PHONY: install-docs
install-docs:
	mkdir -p $(TDOC)
	(									\
		cd $(SRC)/doc &&						\
		$(CP) changelog cmcat examples license README $(TDOC)/		\
	)
	rm -rf $(TDOC)/user-guide
	$(CP) $(SRC)/doc/user-guide/main $(TDOC)/user-guide
	$(GZIP) -c $(SRC)/doc/user-guide/main.ps >$(TDOC)/user-guide.ps.gz
	for f in callcc command-line hello-world same-fringe signals	\
			size taut thread1 thread2 thread-switch timeout \
		; do 							\
		$(CP) $(SRC)/regression/$$f.sml $(TEXM)/; 		\
	done
	$(GZIP) -c $(LEX)/$(LEX).ps >$(TDOC)/$(LEX).ps.gz
	$(GZIP) -c $(YACC)/$(YACC).ps >$(TDOC)/$(YACC).ps.gz
	find $(TDOC)/ -name CVS -type d | xargs rm -rf
	find $(TDOC)/ -name .cvsignore -type f | xargs rm -rf
	find $(TEXM)/ -name CVS -type d | xargs rm -rf
	find $(TEXM)/ -name .cvsignore -type f | xargs rm -rf

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
