SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
TARGET = self
FLAGS = -target $(TARGET)
NAME = mlyacc
PATH = $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH=$(BIN):$$PATH && $(MLTON) -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(FLAGS) $(NAME).mlb
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		mllex src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/yacc.grm.sig src/yacc.grm.sml: src/yacc.grm
	rm -f src/yacc.grm.* && \
		mlyacc src/yacc.grm && \
		chmod -w src/yacc.grm.*

doc/mlyacc.ps:
	$(MAKE) -C doc mlyacc.ps

mlyacc.ps: doc/mlyacc.ps
	cp doc/mlyacc.ps .

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
