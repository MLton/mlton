## Copyright (C) 2009,2013,2018-2021 Matthew Fluet.
 # Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

NAME := mlyacc

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell $(MLBDEPS) $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	"$(RUN_MLTON)" @MLton $(RUN_MLTON_RUNTIME_ARGS) -- $(RUN_MLTON_COMPILE_ARGS) -target $(TARGET) $(NAME).mlb

.PHONY: clean
clean:
	$(SRC)/bin/clean

ifeq (true,$(call HAVE_CMD,$(RUN_MLLEX)))
src/%.lex.sml: src/%.lex
	$(RM) $<.*
	$(RUN_MLLEX) $<
	$(CHMOD) -w $<.*
	$(CP) $<.sml $<.sml.boot
else
src/%.lex.sml: src/%.lex.sml.boot
	$(RM) $@
	$(CP) $< $@
	$(CHMOD) -w $@
endif

ifeq (true,$(call HAVE_CMD,$(RUN_MLYACC)))
src/%.grm.sig src/%.grm.sml: src/%.grm
	$(RM) $<.*
	$(RUN_MLYACC) $<
	$(CHMOD) -w $<.*
	$(CP) $<.sig $<.sig.boot
	$(CP) $<.sml $<.sml.boot
else
src/%.grm.sig: src/%.grm.sig.boot
	$(RM) $@
	$(CP) $< $@
	$(CHMOD) -w $@
src/%.grm.sml: src/%.grm.sml.boot
	$(RM) $@
	$(CP) $< $@
	$(CHMOD) -w $@
endif


PDFLATEX := pdflatex

doc/mlyacc.pdf:
	$(MAKE) -C doc mlyacc.pdf

mlyacc.pdf: doc/mlyacc.pdf
	$(CP) doc/mlyacc.pdf .

DOCS :=
ifeq (true, $(call HAVE_CMD,$(PDFLATEX)))
DOCS += mlyacc.pdf
endif

.PHONY: docs
docs: $(DOCS)


.PHONY: test
test: $(NAME)
	$(CP) ../mlton/front-end/ml.grm .			\
	$(NAME) ml.grm &&					\
	$(DIFF) ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	$(DIFF) ml.grm.sml ../mlton/front-end/ml.grm.sml	\
	$(RM) ml.grm ml.grm.sig ml.grm.sml ml.grm.desc
