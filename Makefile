ROOT = $(shell cd .. && pwd)
BIN = $(ROOT)/bin
INC = $(ROOT)/include
LIB = $(ROOT)/lib
SRC = $(ROOT)/src
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
RUNTIME = $(LIB)/libmlton.a $(LIB)/libmlton-gdb.a $(LIB)/libgmp.a \
	$(INC)/mlton.h
AOUT = mlton-compile
LEX = mllex
PROF = mlprof
YACC = mlyacc
PATH = $(BIN):$(shell echo $$PATH)
CP = /bin/cp -fp

all:	$(MLTON) $(BIN)/$(LEX) $(BIN)/$(PROF) $(BIN)/$(YACC)
	cd $(SRC)/doc/user-guide && $(MAKE)
	chmod a-w $(INC)/*
	@echo 'Build of MLton succeeded'

$(BIN)/$(LEX): $(LEX)/$(LEX)
	$(CP) $(LEX)/$(LEX) $(BIN)

$(LEX)/$(LEX): $(MLTON) 
	cd $(LEX) && $(MAKE) clean && $(MAKE)

$(BIN)/$(PROF): $(PROF)/$(PROF)
	$(CP) $(PROF)/$(PROF) $(BIN)

$(PROF)/$(PROF): $(MLTON) 
	cd $(PROF) && $(MAKE) clean && $(MAKE)

$(BIN)/$(YACC): $(YACC)/$(YACC)
	$(CP) $(YACC)/$(YACC) $(BIN)

$(YACC)/$(YACC): $(MLTON) $(BIN)/$(LEX)
	cd $(YACC) && $(MAKE) clean && $(MAKE)

.PHONY: runtime
runtime: $(RUNTIME)

$(MLTON): bin/mlton $(LIB)/$(AOUT) $(LIB)/world.mlton $(RUNTIME)
	@echo 'Setting root path in mlton script'
	rm -f $(MLTON)
	sed "/^root=/s;'.*';'$(ROOT)';" <bin/mlton >$(MLTON)
	chmod a+x-w $(MLTON) 

$(LIB)/$(AOUT): $(COMP)/$(AOUT)
	$(CP) $(COMP)/$(AOUT) $(LIB)

$(COMP)/$(AOUT):
	cd $(COMP) && $(MAKE)

$(LIB)/libmlton.a: $(RUN)/libmlton.a
	$(CP) $(RUN)/libmlton.a $(LIB)

$(LIB)/libmlton-gdb.a: $(RUN)/libmlton-gdb.a
	$(CP) $(RUN)/libmlton-gdb.a $(LIB)

$(LIB)/libgmp.a: $(RUN)/libgmp.a
	$(CP) $(RUN)/libgmp.a $(LIB)

$(RUN)/libmlton.a $(RUN)/libmlton-gdb.a $(RUN)/libgmp.a:
	@echo 'Compiling MLton runtime system'
	cd runtime && $(MAKE)

$(INC)/mlton.h: include/mlton.h runtime/*.h
	$(CP) include/*.h $(INC)
	$(CP) runtime/*.h $(INC)

.PHONY: world
world: 
	$(LIB)/$(AOUT) $(SRC)/basis-library $(LIB)/world

$(LIB)/world.mlton: $(LIB)/$(AOUT) \
		$(shell find basis-library | egrep '*.(fun|sml|sig)$$')
	@echo 'Processing basis library'
	$(MAKE) world

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
	mkdir -p $(TDOC) $(TBIN) $(TLIB)/lib $(TLIB)/include $(TMAN) &&	\
	(								\
		cd $(SRC)/doc &&					\
		$(CP) -r CHANGES cmcat.sml examples license README 	\
			$(TDOC) && 					\
		mv user-guide/main $(TDOC)/HTML &&			\
		gzip -c user-guide/main.ps >$(TDOC)/user-guide.ps.gz	\
	) &&								\
	(								\
		cd $(LIB) &&						\
		$(CP) *.a $(AOUT) world.mlton $(TLIB)/lib		\
	) &&								\
	(								\
		cd $(INC) &&						\
		$(CP) *.h $(TLIB)/include &&				\
		chmod u+w $(TLIB)/include/*				\
	) &&								\
	(								\
		cd $(BIN) &&						\
		sed "/^root=/s;'.*';'$(ULIB)';" <mlton >$(TBIN)/mlton &&	\
		chmod +x $(TBIN)/mlton &&				\
		$(CP) $(LEX) $(PROF) $(YACC) $(TBIN)			\
	) &&								\
	(								\
		cd $(SRC)/man &&					\
		$(CP) mlton.1 mlprof.1 $(TMAN)				\
	)
