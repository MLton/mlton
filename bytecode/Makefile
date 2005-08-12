## Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

TARGET_ARCH = $(shell ../bin/host-arch)

all: interpret.o interpret-gdb.o print-opcodes

CC = gcc -std=gnu99
CFLAGS = -fomit-frame-pointer -I../runtime -I../include -Wall

ifeq ($(TARGET_ARCH), amd64)
CFLAGS += -mtune=opteron -m32 
endif 

interpret.o: interpret.c interpret.h opcode.h
	$(CC) $(CFLAGS) -c -O2 interpret.c

interpret-gdb.o: interpret.c interpret.h
	$(CC) $(CFLAGS) -c -o $@ -gstabs+ -g2 -DASSERT=1 interpret.c

print-opcodes: print-opcodes.c opcode.h
	$(CC) $(CFLAGS) -o print-opcodes -I../runtime -L../runtime	\
		print-opcodes.c -lmlton

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	$(CC) -E -I../runtime -I../include interpret.c >/tmp/z.exp
