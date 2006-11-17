## Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

TARGET_ARCH = $(shell ../bin/host-arch)

all: interpret.o interpret-gdb.o print-opcodes

CC = gcc -std=gnu99
CFLAGS = -fomit-frame-pointer -I../runtime -I../include -Wall
DEBUGFLAGS = $(CFLAGS)
ifneq ($(TARGET_ARCH), ia64)
ifneq ($(TARGET_ARCH), powerpc)
DEBUGFLAGS += -gstabs+
endif
endif
DEBUGFLAGS += -g2

ifeq ($(TARGET_ARCH), amd64)
CFLAGS += -mtune=opteron -m32 
endif 

ifeq ($(TARGET_OS), darwin)
CFLAGS += -I/sw/include
endif

ifeq ($(TARGET_OS), freebsd)
CFLAGS += -I/usr/local/include
endif

interpret.o: interpret.c interpret.h opcode.h
	$(CC) $(CFLAGS) -c -O2 interpret.c

interpret-gdb.o: interpret.c interpret.h
	$(CC) $(DEBUGFLAGS) -c -o $@ -DASSERT=1 interpret.c

print-opcodes: print-opcodes.c opcode.h
	$(CC) $(CFLAGS) -o print-opcodes -I../runtime -L../runtime	\
		print-opcodes.c -lmlton

.PHONY: clean
clean:
	../bin/clean

.PHONY: exp
exp:
	$(CC) -E -I../runtime -I../include interpret.c >/tmp/z.exp
