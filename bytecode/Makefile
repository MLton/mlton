all: interpret.o print-opcodes

interpret.o: interpret.c interpret.h
	gcc -Wall -c -I../runtime -I../include interpret.c

print-opcodes: print-opcodes.c opcode.h
	gcc -o print-opcodes -I../runtime print-opcodes.c 

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	gcc -E -I../runtime -I../include interpret.c >/tmp/z.exp
	
