ROOT = $(shell cd .. && pwd)
BIN = $(ROOT)/bin
INC = $(ROOT)/include
LIB = $(ROOT)/lib
SRC = $(ROOT)/src
COMP = $(SRC)/mlton
RUN = $(SRC)/runtime
MLTON = $(BIN)/mlton
RUNTIME = $(LIB)/libmlton.a $(LIB)/libmlton-gdb.a $(LIB)/libgmp.a \
	$(LIB)/prof.o $(INC)/mlton.h
AOUT = mlton-compile
LEX = mllex
PROF = mlprof
YACC = mlyacc
PATH = $(BIN):$(shell echo $$PATH)

all:	$(MLTON) $(BIN)/$(LEX) $(BIN)/$(PROF) $(BIN)/$(YACC)
	cd $(SRC)/doc/user-guide && $(MAKE)
	chmod a-w $(INC)/*
	@echo 'Build of MLton succeeded'

$(BIN)/$(LEX): $(LEX)/$(LEX)
	cp -p $(LEX)/$(LEX) $(BIN)

$(LEX)/$(LEX): $(MLTON) 
	cd $(LEX) && $(MAKE) clean && $(MAKE)

$(BIN)/$(PROF): $(PROF)/$(PROF)
	cp -p $(PROF)/$(PROF) $(BIN)

$(PROF)/$(PROF): $(MLTON) 
	cd $(PROF) && $(MAKE) clean && $(MAKE)

$(BIN)/$(YACC): $(YACC)/$(YACC)
	 cp -p $(YACC)/$(YACC) $(BIN)

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
	cp -p $(COMP)/$(AOUT) $(LIB)

$(COMP)/$(AOUT):
	cd $(COMP) && $(MAKE)

$(LIB)/libmlton.a: $(RUN)/libmlton.a
	cp -p $(RUN)/libmlton.a $(LIB)

$(LIB)/libmlton-gdb.a: $(RUN)/libmlton-gdb.a
	cp -p $(RUN)/libmlton-gdb.a $(LIB)

$(LIB)/libgmp.a: $(RUN)/libgmp.a
	cp -p $(RUN)/libgmp.a $(LIB)

$(RUN)/libmlton.a $(RUN)/libmlton-gdb.a $(RUN)/libgmp.a:
	@echo 'Compiling MLton runtime system'
	cd runtime && $(MAKE)

$(LIB)/prof.o: $(RUN)/prof.o
	cp -p $(RUN)/prof.o $(LIB)

$(INC)/mlton.h: include/mlton.h runtime/*.h
	cp -fp include/*.h $(INC)
	cp -fp runtime/*.h $(INC)

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
	for d in basis-library benchmark bin include lib man mllex mlprof \
			mlton mlyacc regression runtime; do 		\
		cd $$d && $(MAKE) clean && cd ..;			\
	done

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
		cp -pr CHANGES cmcat.sml examples license README 	\
			$(TDOC) && 					\
		mv user-guide/main $(TDOC)/HTML &&			\
		gzip -c user-guide/main.ps >$(TDOC)/user-guide.ps.gz	\
	) &&								\
	(								\
		cd $(LIB) &&						\
		cp -p *.a prof.o $(AOUT) world.mlton $(TLIB)/lib	\
	) &&								\
	(								\
		cd $(INC) &&						\
		cp -p *.h $(TLIB)/include &&				\
		chmod u+w $(TLIB)/include/*				\
	) &&								\
	(								\
		cd $(BIN) &&						\
		sed "/^root=/s;'.*';'$(ULIB)';" <mlton >$(TBIN)/mlton &&	\
		chmod +x $(TBIN)/mlton &&				\
		cp -p $(LEX) $(PROF) $(YACC) $(TBIN)			\
	) &&								\
	(								\
		cd $(SRC)/man &&					\
		cp -p mlton.1 mlprof.1 $(TMAN)				\
	)
