SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
HOST = self
FLAGS = -host $(HOST)
NAME = mlyacc
PATH = $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY:	$(NAME)_cm
$(NAME)_cm: $(NAME)-stubs_cm
	grep -v mlton-stubs $(NAME)-stubs.cm >$(NAME).cm

.PHONY:	$(NAME)-stubs_cm
$(NAME)-stubs_cm: src/yacc.lex.sml src/yacc.grm.sig src/yacc.grm.sml
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'basis-stubs' | 		\
			grep -v 'mlton-stubs-in-smlnj' &&		\
		echo 'call-main.sml';					\
	) >$(NAME)-stubs.cm

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		mllex src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/yacc.grm.sig src/yacc.grm.sml: src/yacc.grm
	rm -f src/yacc.grm.* && \
		mlyacc src/yacc.grm && \
		chmod -w src/yacc.grm.*

doc/mlyacc.dvi:
	cd doc && latex mlyacc.tex && latex mlyacc.tex && latex mlyacc.tex

mlyacc.ps: doc/mlyacc.dvi
	dvips -o mlyacc.ps doc/mlyacc.dvi

.PHONY: clean
clean:
	../bin/clean

.PHONY: docs
docs: mlyacc.ps

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm . &&			\
	$(NAME) ml.grm &&					\
	diff ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	diff ml.grm.sml ../mlton/front-end/ml.grm.sml
