all: interpret.o interpret-gdb.o print-opcodes

CC = gcc -fomit-frame-pointer -I../runtime -I../include -Wall

interpret.o: interpret.c interpret.h opcode.h
	$(CC) -c -O2 interpret.c

interpret-gdb.o: interpret.c interpret.h
	$(CC) -c -o $@ -gstabs+ -g2 -DASSERT=1 interpret.c

print-opcodes: print-opcodes.c opcode.h
	$(CC) -o print-opcodes -I../runtime -L../runtime	\
		print-opcodes.c -lmlton

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	gcc -E -I../runtime -I../include interpret.c >/tmp/z.exp
