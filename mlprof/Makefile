SRC = $(shell cd .. && pwd)
BUILD = $(SRC)/build
BIN = $(BUILD)/bin
LIB = $(BUILD)/lib
MLTON = mlton
TARGET = self
FLAGS = -target $(TARGET)
NAME = mlprof
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

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	./mlprof -d 2 mlton.prof mlmon.out
