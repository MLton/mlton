all: interpret.o interpret-gdb.o print-opcodes

INCLUDES = -I../runtime -I../include

interpret.o: interpret.c interpret.h
	gcc -E -o /tmp/z.out $(INCLUDES) interpret.c
	gcc -Wall -c $(INCLUDES) interpret.c

interpret-gdb.o: interpret.c interpret.h
	gcc -o $@ -gstabs+ -g2 -DASSERT=1 -Wall -c $(INCLUDES) interpret.c

print-opcodes: print-opcodes.c opcode.h
	gcc -o print-opcodes -I../runtime print-opcodes.c 

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	gcc -E -I../runtime -I../include interpret.c >/tmp/z.exp
	
