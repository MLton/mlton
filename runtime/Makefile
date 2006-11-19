## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH = ../bin:$(shell echo $$PATH)

TARGET = self
TARGET_ARCH = $(shell ../bin/host-arch)
TARGET_OS = $(shell ../bin/host-os)
GCC_VERSION = $(shell gcc -v 2>&1 | grep 'gcc version' | sed 's/.*gcc version \(.\).*/\1/')

FLAGS =
OPTFLAGS = -fomit-frame-pointer

ifeq ($(TARGET_ARCH), x86)
ifneq ($(findstring $(GCC_VERSION), 3 4),)
OPTFLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
OPTFLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
endif
endif

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -m32
OPTFLAGS += -mtune=opteron
endif

ifeq ($(TARGET_ARCH), sparc)
FLAGS += -mcpu=v8 -m32
endif

ifeq ($(TARGET_OS), darwin)
FLAGS += -I/sw/include
endif

ifeq ($(TARGET_OS), freebsd)
FLAGS += -I/usr/local/include
endif

ifeq ($(TARGET_OS), solaris)
FLAGS += -Wa,-xarch=v8plusa -funroll-all-loops -mcpu=ultrasparc
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
CFLAGS = -O2 -Wall -I. -Iplatform -D_FILE_OFFSET_BITS=64 $(FLAGS) $(OPTFLAGS)
DEBUGFLAGS = $(CFLAGS)
ifneq ($(TARGET_ARCH), ia64)
ifneq ($(TARGET_ARCH), powerpc)
DEBUGFLAGS += -gstabs+
endif
endif
DEBUGFLAGS += -g2

CFILES = 							\
	$(shell find basis -type f | grep '\.c$$' | grep -v Real/)	\
	$(shell find Posix -type f | grep '\.c$$')		\
	gc.c							\
	platform.c

HFILES = 				\
	gc.h				\
	types.h				\
	platform.h			\
	platform/$(TARGET_OS).h

FILES = $(basename $(CFILES))

# EXTRA_CFILES is for files that we don't want compiled in the big
# lump when compiling COMPILE_FAST.
# Real/*.c can't be there because gcc -O2 messes some of them up.
EXTRA_CFILES =			\
	$(shell find basis/Real -type f | grep '\.c$$')	\
	platform/$(TARGET_OS).c

EXTRA_FILES = $(basename $(EXTRA_CFILES))

ifeq ($(COMPILE_FAST), yes)
  OBJS = runtime.o
  DEBUG_OBJS = runtime-gdb.o
else
  OBJS = $(foreach f, $(FILES), $(f).o)
  DEBUG_OBJS = $(foreach f, $(FILES), $(f)-gdb.o)
endif

OBJS += $(foreach f, $(EXTRA_FILES), $(f).o)
DEBUG_OBJS += $(foreach f, $(EXTRA_FILES), $(f)-gdb.o)

all:  libgdtoa.a libmlton.a libmlton-gdb.a

# When compiling gdtoa, we use defines to replace strto{d,f} with
# gdtoa_strto{d,f} to avoid conflicts with the C library on some
# platforms that define their own strto{d,f}.

libgdtoa.a: gdtoa/arith.h
	cd gdtoa && 					\
		$(CC) $(CFLAGS) 			\
			-Dstrtod=gdtoa_strtod		\
			-Dstrtof=gdtoa_strtof		\
			-w -O1 -c -DINFNAN_CHECK 	\
			*.c
	$(AR) libgdtoa.a gdtoa/*.o
	$(RANLIB) libgdtoa.a

gdtoa/arithchk.c:
	gzip -dc gdtoa.tgz | tar xf -	
	patch -p0 <gdtoa-patch

gdtoa/arithchk.out: gdtoa/arithchk.c
	cd gdtoa && $(CC) -o arithchk.out arithchk.c

gdtoa/arith.h: gdtoa/arithchk.out
	cd gdtoa && ./arithchk.out >arith.h

libmlton.a: $(OBJS) 
	$(AR) libmlton.a $(OBJS)
	$(RANLIB) libmlton.a	

libmlton-gdb.a: $(DEBUG_OBJS)
	$(AR) libmlton-gdb.a $(DEBUG_OBJS)
	$(RANLIB) libmlton-gdb.a

runtime.c: $(CFILES)
	cat $(CFILES) >runtime.c

# It looks like we don't follow the C spec w.r.t. aliasing.  And gcc
# -O2 catches us on the code in Real/*.c where we treat a double as a
# chunk of two words.  Files that have been known to cause problems
# are class.c and gdtoa.c.  But there may be others.  So, we compile
# with -fno-strict-aliasing to prevent gcc from taking advantage of
# this aspect of the C spec.
basis/Real/%-gdb.o: basis/Real/%.c gdtoa/arith.h
	$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -c -o $@ $<
basis/Real/%.o: basis/Real/%.c gdtoa/arith.h
	$(CC) $(CFLAGS) -O1 -fno-strict-aliasing -c -o $@ $<

%-gdb.o: %.c $(HFILES)
	$(CC) $(DEBUGFLAGS) -O1 -DASSERT=1 -c -o $@ $<

%.o: %.c $(HFILES)
	$(CC) $(CFLAGS) -c -o $@ $<

%-gdb.o: %.S
	$(CC) $(DEBUGFLAGS) -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	../bin/clean

.PHONY: gdtoa-patch
gdtoa-patch:
	cd gdtoa && $(MAKE) clean && rm -f &~
	mv gdtoa gdtoa-new
	gzip -dc gdtoa.tgz | tar xf -
	diff -P -C 2 -r gdtoa gdtoa-new >gdtoa-patch || exit 0
	rm -rf gdtoa
	mv gdtoa-new gdtoa
