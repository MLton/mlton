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
HOSTMAP=$(LIB)/hostmap
LEX = mllex
PROF = mlprof
YACC = mlyacc
PATH = $(BIN):$(shell echo $$PATH)
CP = /bin/cp -fp
VERSION = $(shell echo `date +%Y%m%d`)

.PHONY: all
all:
	$(MAKE) compiler dirs
	$(CP) $(COMP)/$(AOUT) $(LIB)
	$(MAKE) script world runtime hostmap constants tools docs
	@echo 'Build of MLton succeeded.'

.PHONY: clean
clean:
	$(MAKE) -C regression clean
	bin/clean

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
		doc/user-guide/macros.tex				\
		doc/CHANGES 						\
		doc/README						\
		mlton/control/control.sml; 				\
	do								\
		sed "s/\(.*\)VERSION\(.*\)/\1$(VERSION)\2/" <$$f >z &&	\
		mv z $$f;						\
	done

.PHONY: world
world: 
	@echo 'Processing basis library.'
	$(LIB)/$(AOUT) @MLton -- $(SRC)/basis-library $(LIB)/world

# The TBIN and TLIB are where the files are going to be after installing.
# The PREFIX is added onto them to indicate where the Makefile actually
# puts them.  (PREFIX is mainly used when building RPMs.)
prefix = /usr/local
TBIN = $(DESTDIR)$(prefix)/bin
ULIB = lib/mlton
TLIB = $(DESTDIR)$(prefix)/$(ULIB)
TMAN = $(DESTDIR)$(prefix)/share/man/man1
TDOC = $(DESTDIR)$(prefix)/share/doc/mlton

.PHONY: install
install:
	mkdir -p $(TDOC) $(TLIB) $(TBIN) $(TMAN)
	(								\
		cd $(SRC)/doc &&					\
		$(CP) -r CHANGES cmcat.sml examples			\
			license README $(TDOC)/ &&			\
		rm -rf $(TDOC)/user-guide &&				\
		cp -a user-guide/main $(TDOC)/user-guide &&		\
		gzip -c user-guide/main.ps >$(TDOC)/user-guide.ps.gz	\
	)
	$(CP) -r $(LIB)/. $(TLIB)
	(								\
		cd $(BIN) &&						\
		sed "/^lib=/s;\".*\";'$(prefix)/$(ULIB)';" 		\
			<mlton >$(TBIN)/mlton &&			\
		chmod +x $(TBIN)/mlton &&				\
		$(CP) $(LEX) $(PROF) $(YACC) $(TBIN)/			\
	)
	(								\
		cd $(SRC)/man &&					\
		$(CP) mlton.1 mlprof.1 $(TMAN)/				\
	)
