## Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH := ../bin:$(shell echo $$PATH)

TARGET := self
TARGET_ARCH := $(shell ../bin/host-arch)
TARGET_OS := $(shell ../bin/host-os)
GCC_MAJOR_VERSION :=						\
	$(shell gcc -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1/')
GCC_MINOR_VERSION :=						\
	$(shell gcc -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\2/')
GCC_VERSION := $(GCC_MAJOR_VERSION).$(GCC_MINOR_VERSION)

FLAGS :=
EXE :=
OPTFLAGS := -O2 -fomit-frame-pointer
GCOPTFLAGS :=
DEBUGFLAGS := -O1 -fno-inline -fkeep-inline-functions -g2
GCDEBUGFLAGS :=
WARNFLAGS :=
OPTWARNFLAGS :=
DEBUGWARNFLAGS :=

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -m64
ifeq ($(findstring $(GCC_VERSION), 3.4 4.0 4.1),$(GCC_VERSION))
GCOPTFLAGS += --param inline-unit-growth=75 --param max-inline-insns-single=1000
endif
DEBUGFLAGS += -gstabs+
OPTWARNFLAGS += -Winline
endif

ifeq ($(TARGET_ARCH), ia64)
FLAGS += -fgnu89-inline
endif

ifeq ($(TARGET_ARCH), sparc)
FLAGS += -m32 -mcpu=v8 -Wa,-xarch=v8plusa
endif

ifeq ($(TARGET_ARCH), x86)
ifeq ($(findstring $(GCC_MAJOR_VERSION), 3 4),$(GCC_MAJOR_VERSION))
OPTFLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
OPTFLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
endif
ifeq ($(findstring $(GCC_VERSION), 3.3 3.4 4.0),$(GCC_VERSION))
GCOPTFLAGS += --param max-inline-insns-single=1000
ifeq ($(findstring $(GCC_VERSION), 3.4 4.0),$(GCC_VERSION))
GCOPTFLAGS += --param inline-unit-growth=200
GCOPTFLAGS += --param large-function-growth=2200
OPTWARNFLAGS += -Winline
endif
endif
ifeq ($(findstring $(GCC_VERSION), 4.1),$(GCC_VERSION))
OPTWARNFLAGS += -Winline
endif
DEBUGFLAGS += -gstabs+
endif

ifeq ($(TARGET_OS), cygwin)
EXE := .exe
endif

ifeq ($(TARGET_OS), darwin)
FLAGS += -I/usr/local/include -I/sw/include -I/opt/local/include
endif

ifeq ($(TARGET_OS), freebsd)
FLAGS += -I/usr/local/include
endif

ifeq ($(TARGET_OS), mingw)
EXE := .exe
endif

ifeq ($(TARGET_OS), netbsd)
FLAGS += -I/usr/pkg/include
endif

ifeq ($(TARGET_OS), openbsd)
FLAGS += -I/usr/local/include
endif

ifeq ($(TARGET_OS), solaris)
FLAGS += -funroll-all-loops
endif

ifeq ($(TARGET), self)
AR := ar rc
RANLIB := ranlib
else
AR := $(TARGET)-ar rc
RANLIB := $(TARGET)-ranlib
FLAGS += -b $(TARGET)
endif

CC := gcc -std=gnu99
CPPFLAGS :=
CFLAGS := -I. -Iplatform $(FLAGS)
OPTCFLAGS := $(CFLAGS) $(CPPFLAGS) $(OPTFLAGS)
DEBUGCFLAGS := $(CFLAGS) $(CPPFLAGS) -DASSERT=1 $(DEBUGFLAGS)
GCOPTCFLAGS = $(GCOPTFLAGS)
GCDEBUGCFLAGS = $(GCDEBUGFLAGS)
WARNCFLAGS :=
WARNCFLAGS += -pedantic -Wall
ifeq ($(findstring $(GCC_MAJOR_VERSION), 3),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -W
endif
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wextra
endif
WARNCFLAGS += -Wformat=2
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wswitch-default -Wswitch-enum
endif
WARNCFLAGS += -Wuninitialized
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Winit-self
endif
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wstrict-aliasing=2
endif
WARNCFLAGS += -Wfloat-equal
WARNCFLAGS += -Wundef
WARNCFLAGS += -Wshadow
WARNCFLAGS += -Wpointer-arith
WARNCFLAGS += -Wbad-function-cast -Wcast-qual -Wcast-align
WARNCFLAGS += -Wwrite-strings
# WARNCFLAGS += -Wconversion
WARNCFLAGS += -Waggregate-return
WARNCFLAGS += -Wstrict-prototypes
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wold-style-definition
endif
WARNCFLAGS += -Wmissing-prototypes -Wmissing-declarations
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wmissing-field-initializers
endif
WARNCFLAGS += -Wmissing-noreturn
WARNCFLAGS += -Wmissing-format-attribute
# WARNCFLAGS += -Wpacked
# WARNCFLAGS += -Wpadded
WARNCFLAGS += -Wredundant-decls
WARNCFLAGS += -Wnested-externs
WARNCFLAGS += -Wlong-long
# WARNCFLAGS += -Wunreachable-code
WARNCFLAGS += $(WARNFLAGS)

# GCC doesn't recognize the %I64 format specifier which means %ll on windows
ifeq ($(TARGET_OS), mingw)
WARNCFLAGS += -Wno-format -Wno-missing-format-attribute
endif

OPTWARNCFLAGS := $(WARNCFLAGS) -Wdisabled-optimization $(OPTWARNFLAGS)
DEBUGWARNCFLAGS := $(WARNCFLAGS) $(DEBUGWARNFLAGS)

UTILHFILES :=							\
	util.h							\
	$(shell find util -type f | grep '\.h$$')
UTILCFILES :=							\
	$(shell find util -type f | grep '\.c$$')

PLATFORMHFILES :=						\
	platform.h						\
	$(shell find platform -type f | grep '\.h$$')

GCHFILES :=							\
	gc.h							\
	$(shell find gc -type f | grep '\.h$$')
GCCFILES :=							\
	$(shell find gc -type f | grep '\.c$$')

BYTECODEHFILES :=						\
	$(shell find bytecode -type f | grep '\.h$$')

BASISHFILES :=							\
	ml-types.h						\
	c-types.h						\
	basis-ffi.h						\
	$(shell find basis -type f | grep '\.h$$')
BASISCFILES :=							\
	$(shell find basis -type f | grep '\.c$$')

HFILES :=							\
	cenv.h							\
	$(UTILHFILES)						\
	$(PLATFORMHFILES)					\
	$(GCHFILES)						\
	$(BASISHFILES)

OBJS := 							\
	util.o							\
	gc.o							\
	platform.o						\
	platform/$(TARGET_OS).o
DEBUG_OBJS := 							\
	util-gdb.o						\
	gc-gdb.o						\
	platform-gdb.o						\
	platform/$(TARGET_OS)-gdb.o

OMIT_BYTECODE := no
ifeq ($(OMIT_BYTECODE), yes)
else
  OBJS += bytecode/interpret.o
  DEBUG_OBJS += bytecode/interpret-gdb.o
endif

ifeq ($(COMPILE_FAST), yes)
  OBJS += basis.o
  DEBUG_OBJS += basis-gdb.o
else
  OBJS += 							\
	$(foreach f, $(basename $(BASISCFILES)), $(f).o)
  DEBUG_OBJS += 						\
	$(foreach f, $(basename $(BASISCFILES)), $(f)-gdb.o)
endif

ALL := libgdtoa.a libmlton.a libmlton-gdb.a
ALL += gen/c-types.sml gen/basis-ffi.sml gen/sizes
ifeq ($(OMIT_BYTECODE), yes)
else
  ALL += bytecode/opcodes
endif

all: $(ALL)

libgdtoa.a: gdtoa/arith.h
	cd gdtoa && 						\
		$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS)		\
			-w -O1 -c -DINFNAN_CHECK 		\
			*.c
	$(RM) gdtoa/arithchk.o
	$(AR) libgdtoa.a gdtoa/*.o
	$(RANLIB) libgdtoa.a

gdtoa/arithchk.c:
	gzip -dc gdtoa.tgz | tar xf -
	patch -s -p0 <gdtoa-patch

gdtoa/arithchk.out: gdtoa/arithchk.c
	cd gdtoa && $(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -w -O1 -o arithchk.out arithchk.c

gdtoa/arith.h: gdtoa/arithchk.out
	cd gdtoa && ./arithchk.out >arith.h

libmlton.a: $(OBJS)
	$(AR) libmlton.a $(OBJS)
	$(RANLIB) libmlton.a

libmlton-gdb.a: $(DEBUG_OBJS)
	$(AR) libmlton-gdb.a $(DEBUG_OBJS)
	$(RANLIB) libmlton-gdb.a


basis.c: $(BASISCFILES)
	rm -f basis.c
	cat $(BASISCFILES) >> basis.c

gen/c-types.h gen/c-types.sml gen/ml-types.h: gen/gen-types.c util.h util.o
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o gen/gen-types gen/gen-types.c util.o
	rm -f gen/c-types.h gen/c-types.sml gen/ml-types.h
	cd gen && ./gen-types
	rm -f gen/gen-types$(EXE)

c-types.h ml-types.h: gen/c-types.h gen/ml-types.h
	rm -f c-types.h ml-types.h
	cp gen/c-types.h c-types.h
	cp gen/ml-types.h ml-types.h

gen/basis-ffi.h gen/basis-ffi.sml: gen/gen-basis-ffi.sml gen/basis-ffi.def
	mlton -output gen/gen-basis-ffi gen/gen-basis-ffi.sml
	rm -f gen/basis-ffi.h gen/basis-ffi.sml
	cd gen && ./gen-basis-ffi
	rm -f gen/gen-basis-ffi

basis-ffi.h: gen/basis-ffi.h
	rm -f basis-ffi.h
	cp gen/basis-ffi.h basis-ffi.h

gen/sizes: gen/gen-sizes.c libmlton.a
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o gen/gen-sizes -I. -L. -lmlton gen/gen-sizes.c util.o
	rm -f gen/sizes
	cd gen && ./gen-sizes
	rm -f gen/gen-sizes$(EXE)

bytecode/opcodes: bytecode/print-opcodes.c bytecode/opcode.h
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o bytecode/print-opcodes bytecode/print-opcodes.c
	rm -f bytecode/opcodes
	cd bytecode && ./print-opcodes > opcodes
	rm -f bytecode/print-opcodes$(EXE)


util-gdb.o: util.c $(UTILCFILES) cenv.h $(UTILHFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<

util.o: util.c $(UTILCFILES) cenv.h $(UTILHFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

gc-gdb.o: gc.c $(GCCFILES) $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(GCDEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<

gc.o: gc.c $(GCCFILES) $(HFILES)
	$(CC) $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

## Needs -Wno-float-equal for Real<N>_equal, included via "c-chunk.h".
bytecode/interpret-gdb.o: bytecode/interpret.c $(HFILES) $(BYTECODEHFILES)
	$(CC) -I../include $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -c -o $@ $<

## Needs -Wno-float-equal for Real<N>_equal, included via "c-chunk.h".
bytecode/interpret.o: bytecode/interpret.c $(HFILES) $(BYTECODEHFILES)
	$(CC) -I../include $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -c -o $@ $<


## Needs -Wno-float-equal for Real<N>_equal;
## needs -Wno-format-nonliteralfor Date_strfTime;
## needs -Wno-redundant-decls for 'extern struct GC_state gcState'.
basis-gdb.o: basis.c $(BASISCFILES) $(HFILES)
	$(CC) -Ibasis -Ibasis/Word -Ibasis/Real $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -Wno-format-nonliteral -Wno-redundant-decls -c -o $@ $<
## Needs -Wno-float-equal for Real<N>_equal.
basis/Real/Real-gdb.o: basis/Real/Real.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -c -o $@ $<
## Needs -Wno-format-nonliteralfor Date_strfTime.
basis/System/Date-gdb.o: basis/System/Date.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-format-nonliteral -c -o $@ $<

%-gdb.o: %.c $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<


## Needs -Wno-float-equal for Real<N>_equal;
## needs -Wno-format-nonliteral for Date_strfTime;
## needs -Wno-redundant-decls for 'extern struct GC_state gcState'.
basis.o: basis.c $(BASISCFILES) $(HFILES)
	$(CC) -Ibasis -Ibasis/Word -Ibasis/Real $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -Wno-format-nonliteral -Wno-redundant-decls -c -o $@ $<
## Needs -Wno-float-equal for Real<N>_equal.
basis/Real/Real.o: basis/Real/Real.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -c -o $@ $<
## Needs -Wno-format-nonliteralfor Date_strfTime.
basis/System/Date.o: basis/System/Date.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-format-nonliteral -c -o $@ $<

%.o: %.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<


.PHONY: flags
flags:
	echo TARGET = $(TARGET)
	echo TARGET_ARCH = $(TARGET_ARCH)
	echo TARGET_OS = $(TARGET_OS)
	echo GCC_MAJOR_VERSION = $(GCC_MAJOR_VERSION)
	echo GCC_MINOR_VERSION = $(GCC_MINOR_VERSION)
	echo GCC_VERSION = $(GCC_VERSION)
	echo FLAGS = $(FLAGS)
	echo EXE = $(EXE)
	echo OPTFLAGS = $(OPTFLAGS)
	echo GCOPTFLAGS = $(GCOPTFLAGS)
	echo DEBUGFLAGS = $(DEBUGFLAGS)
	echo WARNFLAGS = $(WARNFLAGS)
	echo OPTWARNFLAGS = $(OPTWARNFLAGS)
	echo DEBUGWARNFLAGS = $(DEBUGWARNFLAGS)
	echo OBJS = $(OBJS)
	echo DEBUG_OBJS = $(DEBUG_OBJS)


.PHONY: clean
clean:
	../bin/clean

.PHONY: gdtoa-patch
gdtoa-patch:
	cd gdtoa && $(MAKE) clean && rm -f *~ *.orig
	mv gdtoa gdtoa-new
	gzip -dc gdtoa.tgz | tar xf -
	diff -P -C 2 -r gdtoa gdtoa-new >gdtoa-patch || exit 0
	rm -rf gdtoa
	mv gdtoa-new gdtoa
