SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
HOST = self
FLAGS = -host $(HOST)
NAME = mllex
PATH = $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(FLAGS) $(NAME).cm
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY:	$(NAME)_cm
$(NAME)_cm: 
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | 					\
			grep -v 'basis-stubs' | 			\
			grep -v 'mlton-stubs' | 			\
			grep -v 'mlton-stubs-in-smlnj' &&		\
		echo 'call-main.sml';					\
	) >$(NAME).cm

lexgen.dvi: lexgen.tex
	latex lexgen.tex; latex lexgen.tex

mllex.ps: lexgen.dvi
	dvips -o mllex.ps lexgen.dvi

.PHONY: clean
clean:
	../bin/clean

.PHONY: docs
docs: mllex.ps

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.lex . &&			\
	$(NAME) ml.lex &&					\
	diff ml.lex.sml ../mlton/front-end/ml.lex.sml
