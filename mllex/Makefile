NAME = mllex
MLTON = mlton
HOST = self
FLAGS = -host $(HOST)
PATH = $(shell cd ../build/bin && echo `pwd`:$$PATH)

all:	$(NAME)

$(NAME): $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	@echo 'Compiling $(NAME)'
	time $(MLTON) $(FLAGS) $(NAME).cm
	strip $(NAME)
	size $(NAME)

$(NAME).sml: $(NAME).cm $(shell $(MLTON) -stop f $(NAME).cm)
	mlton -stop sml $(NAME).cm

.PHONY:	$(NAME)_cm
$(NAME)_cm: $(NAME)-stubs_cm
	grep -v mlton-stubs $(NAME)-stubs.cm >$(NAME).cm

.PHONY:	$(NAME)-stubs_cm
$(NAME)-stubs_cm: 
	(								\
		echo 'Group is'&&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME)-stubs.cm

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.lex . &&			\
	$(NAME) ml.lex &&					\
	diff ml.lex.sml ../mlton/front-end/ml.lex.sml
