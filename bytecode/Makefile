all: interpret.c
	gcc -o interpret 	\
		-I../runtime	\
		-I../include 	\
		interpret.c	\
		../runtime/libmlton.a -lm
	size interpret
	interpret

.PHONY: exp
exp:
	gcc -E -I../runtime -I../include interpret.c >/tmp/z.exp
	
