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
DEFAULT_MODEL = A
ALL_MODELS = A
endif

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -mtune=opteron
DEFAULT_MODEL = BX
ALL_MODELS = A AX B BX C CX G
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
CWFLAGS = -Wall -pedantic -Wextra -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wconversion -Wsign-compare -Wstrict-prototypes -Wredundant-decls -Winline
CWFLAGS = -pedantic -Wall -Wextra -Wno-unused \
##	-Wshadow \
	-Wredundant-decls \
	-Wpointer-arith -Wcast-qual -Wcast-align \
##	-Wconversion \
	-Wstrict-prototypes \
	-Winline
CFLAGS = -O2 $(CWFLAGS) -I. -D_FILE_OFFSET_BITS=64 $(FLAGS)
DEBUGFLAGS = $(CFLAGS) -Wunused -gstabs+ -g2

## Order matters, as these are concatenated together to form "gc.c".
CFILES = 								\
	gc_prefix.c							\
	debug.c								\
	align.c								\
	pointer.c							\
	model.c								\
	object.c							\
	array.c								\
	frame.c								\
	stack.c								\
	thread.c							\
	generational.c							\
	heap.c   							\
	invariant.c   							\
	foreach.c							\
	cheney-copy.c							\
	assumptions.c							\
	gc_suffix.c

## Order matters, as these are concatenated together to form "gc.h".
HFILES = 								\
	gc_prefix.h							\
	util.h								\
	virtual-memory.h						\
	pointer.h							\
	model.h								\
	object.h							\
	array.h								\
	frame.h								\
	stack.h								\
	thread.h							\
	weak.h								\
	major.h								\
	generational.h							\
	statistics.h							\
	heap.h								\
	gc_state.h							\
	gc_suffix.h

all: gc.o gc-gdb.o

gc-gdb.o: gc.c gc.h
	$(CC) $(DEBUGFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -O1 -DASSERT=1 -c -o $@ gc.c

gc.o: gc.c gc.h
	$(CC) $(CFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -c -o $@ gc.c

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

.PHONY: models
models: gc.c gc.h
	(								\
		for m in $(ALL_MODELS); do				\
			$(CC) $(CFLAGS) -DGC_MODEL_$$m -c -o gc.$$m.o gc.c;	\
			$(CC) $(CFLAGS) $(DEBUGFLAGS) -O1 -DASSERT=1 -DGC_MODEL_$$m -c -o gc-gdb.$$m.o gc.c;	\
		done;							\
	)

.PHONY: clean
clean:
	../../bin/clean
