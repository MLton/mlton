NAME = mllex
MLTON = mlton
FLAGS =
PATH = ../../bin:$(shell echo $$PATH)

all:	$(NAME)

$(NAME): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	strip $(NAME)
	size $(NAME)

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
	rm -f junk $(NAME) $(NAME).sml ml.lex ml.lex.sml

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.lex . &&			\
	$(NAME) ml.lex &&					\
	diff ml.lex.sml ../mlton/front-end/ml.lex.sml
