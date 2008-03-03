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

CFILES = 							\
	$(shell find . -type f | grep '\.c$$')

HFILES = 							\
	$(shell find . -type f | grep '\.h$$')

all: libgc.o libgc-gdb.o

libgc-gdb.o: $(CFILES) $(HFILES)
	$(CC) $(DEBUGFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -O1 -DASSERT=1 -c -o $@ gc.c

libgc.o: $(CFILES) $(HFILES)
	$(CC) $(CFLAGS) -DGC_MODEL_$(DEFAULT_MODEL) -c -o $@ gc.c

.PHONY: models
models: $(CFILES) $(HFILES)
	(								\
		for m in $(ALL_MODELS); do				\
			$(CC) $(CFLAGS) -DGC_MODEL_$$m -c -o libgc.$$m.o gc.c;	\
			$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -DGC_MODEL_$$m -c -o libgc-gdb.$$m.o gc.c;	\
		done;							\
	)

.PHONY: clean
clean:
	../../bin/clean
