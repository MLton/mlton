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

ifeq ($(MAKECMDGOALS),$(NAME).cm)
.PHONY:	$(NAME).cm
$(NAME).cm: src/yacc.lex.sml src/yacc.grm.sig src/yacc.grm.sml
else
$(NAME).cm:
endif
	(								\
		echo 'Group is' &&					\
		cmcat sources.cm | grep -v 'mlton-stubs-in-smlnj' &&	\
		echo 'call-main.sml';					\
	) >$(NAME).cm

src/yacc.lex.sml: src/yacc.lex
	rm -f src/yacc.lex && \
		mllex src/yacc.lex && \
		chmod -w src/yacc.lex.sml

src/yacc.grm.sig src/yacc.grm.sml: src/yacc.grm
	rm -f src/yacc.grm.* && \
		mlyacc src/yacc.grm && \
		chmod -w src/yacc.grm.*

.PHONY: clean
clean:
	find . -type f | grep '.*~$$' | xargs rm -f
	find . -type d | grep 'CM$$' | xargs rm -rf
	rm -f junk $(NAME) $(NAME).sml src/yacc.lex.sml src/yacc.grm.* ml.grm*

.PHONY: test
test: $(NAME)
	cp -p ../mlton/front-end/ml.grm . &&			\
	$(NAME) ml.grm &&					\
	diff ml.grm.sig ../mlton/front-end/ml.grm.sig &&	\
	diff ml.grm.sml ../mlton/front-end/ml.grm.sml
