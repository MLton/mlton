export HOST = self
export HOSTTYPE = $(shell bin/hosttype)
ROOT = $(shell pwd)
BUILD = $(ROOT)/build
SRC = $(ROOT)
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
AOUT = mlton-compile
HOSTMAP = $(LIB)/hostmap
SPEC = $(SRC)/doc/mlton.spec
LEX = mllex
PROF = mlprof
YACC = mlyacc
PATH = $(BIN):$(shell echo $$PATH)
CP = /bin/cp -fpR

VERSION = $(shell date +%Y%m%d)
DATE = $(shell date -R)
RELEASE = 1

.PHONY: all
all:
	$(MAKE) compiler dirs
	$(CP) $(COMP)/$(AOUT) $(LIB)/
	$(MAKE) script world runtime hostmap constants tools docs
	@echo 'Build of MLton succeeded.'

.PHONY: bootstrap
bootstrap:
	$(MAKE)
	rm -f mlton/mlton-compile
	$(MAKE)

.PHONY: clean
clean:
	bin/clean

.PHONY: clean-cvs
clean-cvs:
	find . -type d | grep CVS | xargs rm -rf

.PHONY: cm
cm:
	$(MAKE) -C $(COMP) mlton_cm
	$(MAKE) -C $(LEX) mllex_cm
	$(MAKE) -C $(PROF) mlprof_cm
	$(MAKE) -C $(YACC) mlyacc_cm

.PHONY: compiler
compiler:
	$(MAKE) -C $(COMP)

.PHONY: constants
constants:
	@echo 'Creating constants file.'
	$(BIN)/mlton -build-constants >tmp.c
	$(BIN)/mlton -o tmp tmp.c
	./tmp >$(LIB)/$(HOST)/constants
	rm -f tmp tmp.c

.PHONY: deb
deb:
	$(MAKE) clean version
	debuild

.PHONY: deb-binary
deb-binary:
	fakeroot debian/rules binary

.PHONY: deb-lint
deb-lint:
	lintian ../mlton_$(VERSION).1-1_i386.deb

.PHONY: dirs
dirs:
	mkdir -p $(BIN) $(LIB)/$(HOST)/include

.PHONY: docs
docs:
	$(MAKE) -C $(SRC)/doc/user-guide

.PHONY: hostmap
hostmap:
	touch $(HOSTMAP)
	( sed '/$(HOST)/d' <$(HOSTMAP); echo '$(HOST) $(HOSTTYPE)' ) \
		>>$(HOSTMAP).tmp
	mv $(HOSTMAP).tmp $(HOSTMAP)

.PHONY: nj-mlton
nj-mlton:
	$(MAKE) dirs
	$(MAKE) -C $(COMP) nj-mlton
	$(MAKE) script runtime hostmap constants
	@echo 'Build of MLton succeeded.'

.PHONY: nj-mlton-dual
nj-mlton-dual:
	$(MAKE) dirs	
	$(MAKE) -C $(COMP) nj-mlton-dual
	$(MAKE) script runtime hostmap constants
	@echo 'Build of MLton succeeded.'

TOPDIR = 'TOPDIR-unset'
SOURCEDIR = $(TOPDIR)/SOURCES/mlton-$(VERSION)
.PHONY: rpms
rpms:
	mkdir -p $(TOPDIR)
	cd $(TOPDIR) && mkdir -p BUILD RPMS/i386 SOURCES SPECS SRPMS
	rm -rf $(SOURCEDIR)
	mkdir -p $(SOURCEDIR)
	( cd $(SRC) && tar -cpf - . ) | ( cd $(SOURCEDIR) && tar -xpf - )
	cd $(SOURCEDIR) && $(MAKE) clean clean-cvs version
	$(CP) $(SOURCEDIR)/doc/mlton.spec $(TOPDIR)/SPECS/mlton.spec
	( cd $(TOPDIR)/SOURCES && tar -cpf - mlton-$(VERSION) )		\
		| gzip >$(SOURCEDIR).tgz
	rm -rf $(SOURCEDIR)
	rpm -ba --quiet --clean $(TOPDIR)/SPECS/mlton.spec

.PHONY: runtime
runtime:
	@echo 'Compiling MLton runtime system for $(HOST).'
	$(MAKE) -C runtime
	$(CP) $(RUN)/*.a $(LIB)/$(HOST)/
	$(CP) runtime/*.h include/*.h $(LIB)/$(HOST)/include/

.PHONY: script
script:
	@echo 'Setting lib in mlton script.'
	sed "/^lib=/s;'.*';\"\`dirname \$$0\`/../lib\";" <bin/mlton >$(MLTON)
	chmod a+x $(MLTON) 

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
		debian/copyright					\
		doc/mlton.spec						\
		doc/user-guide/macros.tex				\
		mlton/control/control.sml; 				\
	do								\
		sed "s/\(.*\)VERSION\(.*\)/\1$(VERSION)\2/" <$$f >z &&	\
		sed "s/\(.*\)DATE\(.*\)/\1$(DATE)\2/" <z >$$f &&	\
		rm -f z;						\
	done
	sed <$(SPEC) >z "/^Release:/s;.*;Release: $(RELEASE);"
	mv z $(SPEC)


.PHONY: world
world: 
	@echo 'Processing basis library.'
	$(LIB)/$(AOUT) @MLton -- $(SRC)/basis-library $(LIB)/world

# The TBIN and TLIB are where the files are going to be after installing.
# The DESTDIR and is added onto them to indicate where the Makefile actually
# puts them.
DESTDIR = $(CURDIR)/install
prefix = /usr
TBIN = $(DESTDIR)$(prefix)/bin
ULIB = lib/mlton
TLIB = $(DESTDIR)$(prefix)/$(ULIB)
TMAN = $(DESTDIR)$(prefix)/share/man/man1
TDOC = $(DESTDIR)$(prefix)/share/doc/mlton
MANVERS = $(shell date '+%B %d, %Y')

.PHONY: install
install:
	mkdir -p $(TDOC) $(TLIB) $(TBIN) $(TMAN)
	(									\
		cd $(SRC)/doc &&						\
		$(CP) changelog cmcat.sml examples license README $(TDOC)/	\
	)
	rm -rf $(TDOC)/user-guide
	$(CP) $(SRC)/doc/user-guide/main $(TDOC)/user-guide
	gzip -c $(SRC)/doc/user-guide/main.ps >$(TDOC)/user-guide.ps.gz
	for f in callcc command-line hello-world same-fringe signals size taut thread1 thread2 thread-switch timeout; do \
 		cp $(SRC)/regression/$$f.sml $(TDOC)/examples; \
	done
	$(CP) $(LIB)/. $(TLIB)/
	sed "/^lib=/s;'.*';'$(prefix)/$(ULIB)';" 			\
			<$(SRC)/bin/mlton >$(TBIN)/mlton
	chmod +x $(TBIN)/mlton
	$(CP) $(BIN)/$(LEX) $(BIN)/$(PROF) $(BIN)/$(YACC) $(TBIN)/
	for f in mlprof mlton; do						\
		sed "s/\(.*\)VERSION\(.*\)/\1$(MANVERS)\2/" <$(SRC)/man/$$f.1	\
			>$(TMAN)/$$f.1;						\
	done
	find $(DESTDIR) -name CVS -type d | xargs --no-run-if-empty rm -rf
	find $(DESTDIR) -name .cvsignore -type f | xargs --no-run-if-empty rm -rf
