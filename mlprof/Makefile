NAME = mlprof
MLTON = mlton
FLAGS = #-DMLton_debug
PATH = ../../bin:$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	strip $(NAME)
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY:	$(NAME)_cm
$(NAME)_cm:
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME).cm

.PHONY: clean
clean:
	find . -type f | grep '.*~$$' | xargs rm -f
	find . -type d | grep 'CM$$' | xargs rm -rf
	rm -f junk $(NAME) $(NAME).sml

.PHONY: test
test: $(NAME)
	./mlprof -d 2 mlton.prof mlmon.out
