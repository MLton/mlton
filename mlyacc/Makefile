NAME = mlyacc
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
$(NAME)_cm: src/yacc.lex.sml src/yacc.grm.sig src/yacc.grm.sml
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME).cm

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex.sml && \
		mllex src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/yacc.grm.sig src/yacc.grm.sml: src/yacc.grm
	rm -f src/yacc.grm.* && \
		mlyacc src/yacc.grm && \
		chmod -w src/yacc.grm.*

.PHONY: clean
clean:
	../bin/clean

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm . &&			\
	$(NAME) ml.grm &&					\
	diff ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	diff ml.grm.sml ../mlton/front-end/ml.grm.sml
