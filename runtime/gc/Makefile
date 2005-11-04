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

## Order matters, as these are concatenated together to form "libgc.c".
CFILES = 								\
	gc_prefix.c							\
	util.c								\
	safe.c								\
	read_write.c							\
	rusage.c							\
	debug.c								\
	align.c								\
	virtual-memory.c						\
	array-allocate.c						\
	array.c								\
	atomic.c							\
	cheney-copy.c							\
	controls.c							\
	copy-thread.c							\
	current.c   							\
	dfs-mark.c							\
	done.c								\
	enter_leave.c							\
	foreach.c							\
	forward.c							\
	frame.c								\
	garbage-collection.c						\
	gc_state.c   							\
	generational.c							\
	handler.c							\
	hash-cons.c							\
	heap.c								\
	heap_predicates.c						\
	init-world.c							\
	init.c								\
	invariant.c   							\
	mark-compact.c							\
	model.c								\
	model_predicates.c						\
	new-object.c   							\
	object-size.c							\
	object.c							\
	pack.c								\
	pointer.c							\
	pointer_predicates.c						\
	profiling.c							\
	share.c								\
	signals.c							\
	size.c								\
	stack.c								\
	stack_predicates.c						\
	switch-thread.c							\
	thread.c							\
	translate.c							\
	weak.c								\
	world.c								\
	assumptions.c							\
	gc_suffix.c

## Order matters, as these are concatenated together to form "libgc.h".
HFILES = 								\
	gc_prefix.h							\
	util.h								\
	safe.h								\
	rusage.h							\
	virtual-memory.h						\
	model.h								\
	pointer.h							\
	objptr.h							\
	object.h							\
	array.h								\
	frame.h								\
	stack.h								\
	thread.h							\
	weak.h								\
	int-inf.h							\
	object-size.h							\
	generational.h							\
	heap.h								\
	current.h							\
	foreach.h							\
	translate.h							\
	sysvals.h							\
	controls.h							\
	major.h								\
	statistics.h							\
	forward.h							\
	cheney-copy.h							\
	hash-cons.h							\
	dfs-mark.h							\
	mark-compact.h							\
	invariant.h							\
	atomic.h							\
	enter_leave.h							\
	signals.h							\
	handler.h							\
	switch-thread.h							\
	garbage-collection.h						\
	new-object.h							\
	array-allocate.h						\
	profiling.h							\
	init-world.h							\
	world.h								\
	init.h								\
	done.h								\
	copy-thread.h							\
	pack.h								\
	share.h								\
	size.h								\
	gc_state.h							\
	gc_suffix.h

all: libgc.o libgc-gdb.o

libgc-gdb.o: libgc.c libgc.h
	$(CC) $(DEBUGFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -O1 -DASSERT=1 -c -o $@ libgc.c

libgc.o: libgc.c libgc.h
	$(CC) $(CFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -c -o $@ libgc.c

libgc.c: $(CFILES)
	rm -f libgc.c
	(								\
		for f in $(CFILES); do					\
			echo "#line 1 \"$$f\"";				\
			cat $$f;					\
		done;							\
	) > libgc.c

libgc.h: $(HFILES)
	rm -f libgc.h
	(								\
		for f in $(HFILES); do					\
			echo "#line 1 \"$$f\"";				\
			cat $$f;					\
		done;							\
	) > libgc.h

.PHONY: models
models: libgc.c libgc.h
	(								\
		for m in $(ALL_MODELS); do				\
			$(CC) $(CFLAGS) -DGC_MODEL_$$m -c -o libgc.$$m.o libgc.c;	\
			$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -DGC_MODEL_$$m -c -o libgc-gdb.$$m.o libgc.c;	\
		done;							\
	)

.PHONY: clean
clean:
	../../bin/clean
