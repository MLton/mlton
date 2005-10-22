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
CWFLAGS = -pedantic -Wall -Wextra -Wno-unused-parameter -Wno-unused-function \
	-Wformat-nonliteral \
	-Wuninitialized -Winit-self \
	-Wstrict-aliasing=2 \
	-Wfloat-equal \
	-Wpointer-arith \
	-Wbad-function-cast -Wcast-qual -Wcast-align \
	-Waggregate-return \
	-Wstrict-prototypes \
	-Wmissing-noreturn -Wmissing-format-attribute \
	-Wpacked \
	-Wredundant-decls \
	-Wnested-externs 
#	-Wshadow \
#	-Wconversion \
#	-Wmissing-prototypes \
#	-Wmissing-declarations \
#	-Winline -Wdisabled-optimization
CFLAGS = -O2 $(CWFLAGS) -I. -D_FILE_OFFSET_BITS=64 $(FLAGS)
DEBUGFLAGS = $(CFLAGS) -Wunused -gstabs+ -g2

## Order matters, as these are concatenated together to form "gc.c".
CFILES = 								\
	gc_prefix.c							\
	util.c								\
	safe.c								\
	rusage.c							\
	debug.c								\
	align.c								\
	virtual-memory.c						\
	pointer_predicates.c						\
	pointer.c							\
	model_predicates.c						\
	model.c								\
	object.c							\
	array.c								\
	object_size.c							\
	frame.c								\
	stack_predicates.c						\
	stack.c								\
	thread.c							\
	foreach.c							\
	translate.c							\
	generational.c							\
	heap_predicates.c						\
	heap.c								\
	current.c   							\
	ratios_predicates.c						\
	atomic.c							\
	gc_state.c   							\
	invariant.c   							\
	enter_leave.c							\
	cheney-copy.c							\
	hash-cons.c							\
	dfs-mark.c							\
	share.c								\
	mark-compact.c							\
	new_object.c   							\
	garbage-collection.c						\
	array-allocate.c						\
	copy-thread.c							\
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
	heap.h								\
	major.h								\
	generational.h							\
	statistics.h							\
	controls.h							\
	sysvals.h							\
	ratios.h							\
	hash-cons.h							\
	profiling.h							\
	signals.h							\
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
			$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -DGC_MODEL_$$m -c -o gc-gdb.$$m.o gc.c;	\
		done;							\
	)

.PHONY: clean
clean:
	../../bin/clean
