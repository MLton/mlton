ROOT = $(shell pwd)
BUILD = $(ROOT)/build
SRC = $(ROOT)
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
AOUT = mlton-compile
LEX = mllex
PROF = mlprof
YACC = mlyacc
LINUX = linux
CYGWIN = i686-pc-cygwin
PATH = $(BIN):$(shell echo $$PATH)
CP = /bin/cp -fp

all:
	$(MAKE) dirs
	cd $(COMP) && $(MAKE)
	$(CP) $(COMP)/$(AOUT) $(LIB)
	$(MAKE) world
	$(MAKE) runtimes
	$(MAKE) script
	cd $(LEX) && $(MAKE) && $(CP) $(LEX) $(BIN)
	cd $(YACC) && $(MAKE) && $(CP) $(YACC) $(BIN)
	cd $(PROF) && $(MAKE) && $(CP) $(PROF) $(BIN)
	cd $(SRC)/doc/user-guide && $(MAKE)
	@echo 'Build of MLton succeeded'

.PHONY: dirs
dirs:
	mkdir -p $(BIN) $(LIB)/$(LINUX)/include $(LIB)/$(CYGWIN)/include

.PHONY: runtimes
runtimes:
	@echo 'Making runtime system'
	$(MAKE) runtime HOST=$(LINUX)
	$(MAKE) runtime HOST=$(CYGWIN)

.PHONY: runtime
runtime:
	$(MAKE) dirs
	@echo 'Compiling MLton runtime system for $(HOST)'
	cd runtime && $(MAKE) HOST=$(HOST)
	$(CP) $(RUN)/*.a $(LIB)/$(HOST)
	$(CP) runtime/*.h include/*.h $(LIB)/$(HOST)/include
	cd runtime && $(MAKE) clean

.PHONY: script
script:
	@echo 'Setting lib and cygwin in mlton script'
	cat bin/mlton | \
		sed "/^lib=/s;'.*';'$(LIB)';" | \
		sed "/^cygwin=/s;'.*';'$(CYGWIN)';" >$(MLTON)
	chmod a+x $(MLTON) 

.PHONY: world
world: 
	@echo 'Processing basis library'
	$(LIB)/$(AOUT) $(SRC)/basis-library $(LIB)/world

.PHONY: clean
clean:
	bin/clean

# The TBIN and TLIB are where the files are going to be after installing.
# The PREFIX is added onto them to indicate where the Makefile actually
# puts them.  (PREFIX is mainly used when building RPMs.)
PREFIX =
VERSION =
TBIN = $(PREFIX)/usr/local/bin
ULIB = /usr/local/lib/mlton
TLIB = $(PREFIX)/$(ULIB)
TMAN = $(PREFIX)/usr/local/man/man1
TDOC = $(PREFIX)/usr/share/doc/mlton-$(VERSION)

.PHONY: install
install:
	mkdir -p $(TDOC) $(TLIB) $(TBIN) $(TMAN) &&			\
	(								\
		cd $(SRC)/doc &&					\
		$(CP) -r CHANGES build-cross-gcc cmcat.sml examples	\
			license README $(TDOC) &&			\
		mv user-guide/main $(TDOC)/HTML &&			\
		gzip -c user-guide/main.ps >$(TDOC)/user-guide.ps.gz	\
	) &&								\
	( 								\
		cd $(LIB) && $(CP) -r . $(TLIB) 			\
	) &&								\
	(								\
		cd $(BIN) &&						\
		sed "/^lib=/s;'.*';'$(ULIB)';" <mlton >$(TBIN)/mlton &&	\
		chmod +x $(TBIN)/mlton &&				\
		$(CP) $(LEX) $(PROF) $(YACC) $(TBIN)			\
	) &&								\
	(								\
		cd $(SRC)/man &&					\
		$(CP) mlton.1 mlprof.1 $(TMAN)				\
	)
