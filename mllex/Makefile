SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
TARGET = self
FLAGS = -target $(TARGET)
NAME = mllex
PATH = $(BIN):$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).mlb $(shell PATH=$(BIN):$$PATH && $(MLTON) -stop f $(NAME).mlb)
	@echo 'Compiling $(NAME)'
	$(MLTON) $(FLAGS) $(NAME).mlb
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

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
