## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH = ../../bin:$(shell echo $$PATH)

TARGET = self
TARGET_ARCH = $(shell ../../bin/host-arch)
TARGET_OS = $(shell ../../bin/host-os)
GCC_VERSION = $(shell gcc -v 2>&1 | grep 'gcc version' | sed 's/.*gcc version \(.\).*/\1/')

FLAGS = -fomit-frame-pointer

ifeq ($(TARGET_ARCH), x86)
ifneq ($(findstring $(GCC_VERSION), 3 4),)
FLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
FLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
endif
endif

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -mtune=opteron
endif

ifeq ($(TARGET_ARCH), sparc)
FLAGS += -mv8 -m32
endif

ifeq ($(TARGET_OS), solaris)
FLAGS += -Wa,-xarch=v8plusa -fcall-used-g5 -fcall-used-g7 -funroll-all-loops -mcpu=ultrasparc
endif

ifeq ($(TARGET), self)
AR = ar rc
RANLIB = ranlib
else
AR = $(TARGET)-ar rc
RANLIB = $(TARGET)-ranlib
FLAGS += -b $(TARGET)
endif

CC = gcc -std=gnu99
CFLAGS = -O2 -Wall -I. -D_FILE_OFFSET_BITS=64 $(FLAGS)
DEBUGFLAGS = $(CFLAGS) -gstabs+ -g2

CFILES = 								\
	gc_prefix.c							\
	debug.c								\
	object.c							\
	model.c								\
	gc_suffix.c

HFILES = 								\
	gc_prefix.h							\
	util.h								\
	model.h								\
	object.h							\
	stack.h								\
	frame.h								\
	thread.h							\
	weak.h								\
	heap.h								\
	gc_state.h							\
	gc_suffix.h

all: gc.o gc-gdb.o

gc-gdb.o: gc.c gc.h
	$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -c -o $@ gc.c

gc.o: gc.c gc.h
	$(CC) $(CFLAGS) -c -o $@ gc.c

gc.c: $(CFILES)
	rm -f gc.c
	(								\
		for f in $(CFILES); do					\
			echo "#line 1 \"$$f\"";				\
			cat $$f;					\
		done;							\
	) > gc.c

gc.h: $(HFILES)
	rm -f gc.h
	(								\
		for f in $(HFILES); do					\
			echo "#line 1 \"$$f\"";				\
			cat $$f;					\
		done;							\
	) > gc.h

.PHONY: clean
clean:
	../bin/clean
