all: interpret.o print-opcodes

interpret.o: interpret.c
	gcc -Wall -c -I../runtime -I../include interpret.c

print-opcodes: print-opcodes.c interpret.o
	gcc -o print-opcodes -I../runtime print-opcodes.c \
		interpret.o ../runtime/libmlton.a -lm

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	gcc -E -I../runtime -I../include interpret.c >/tmp/z.exp
	
