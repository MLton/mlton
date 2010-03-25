## Copyright (C) 2010 Matthew Fluet.
 # Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH := ../bin:$(shell echo $$PATH)

TARGET := self

ifeq ($(TARGET), self)
CC := gcc -std=gnu99
AR := ar rc
RANLIB := ranlib
else
CC := $(TARGET)-gcc -std=gnu99
AR := $(TARGET)-ar rc
RANLIB := $(TARGET)-ranlib
endif

TARGET_ARCH := $(shell ../bin/host-arch)
TARGET_OS := $(shell ../bin/host-os)
GCC_MAJOR_VERSION :=						\
	$(shell $(CC) -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1/')
GCC_MINOR_VERSION :=						\
	$(shell $(CC) -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\2/')
GCC_VERSION := $(GCC_MAJOR_VERSION).$(GCC_MINOR_VERSION)

FLAGS := -fno-common
EXE :=
OPTFLAGS := -O2 -fomit-frame-pointer
DEBUGFLAGS := -O1 -fno-inline -fkeep-inline-functions -g2
PICFLAGS := -DPIC
GCOPTFLAGS :=
GCDEBUGFLAGS :=
GCPICFLAGS :=
WARNFLAGS :=
OPTWARNFLAGS :=
DEBUGWARNFLAGS :=
PICWARNFLAGS :=

# Win32&64 don't use PIC code, all other platforms do
ifeq ($(findstring $(TARGET_OS), mingw cygwin),)
PICFLAGS += -fPIC
endif

# Make mlton library symbols private (win32&64 use another technique)
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
ifeq ($(findstring $(TARGET_OS), mingw cygwin),)
FLAGS += -fvisibility=hidden
endif
endif

ifeq ($(TARGET_ARCH), alpha)
FLAGS += -mieee -mbwx -mtune=ev6 -mfp-rounding-mode=d
endif

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -m64
DEBUGFLAGS += -gstabs+
endif

ifeq ($(findstring $(TARGET_ARCH), hppa ia64 powerpc sparc),$(TARGET_ARCH))
ifeq (4.2, $(firstword $(sort $(GCC_VERSION) 4.2)))
# GMP headers contain C99 inline functions which generate warnings
# with a suggestion to use this flag to disable the warnings.
FLAGS += -fgnu89-inline
endif
endif

ifeq ($(TARGET_ARCH), ia64)
FLAGS += -mtune=itanium2
ifeq ($(TARGET_OS), hpux)
FLAGS += -mlp64
endif
endif

ifeq ($(TARGET_OS)-$(TARGET_ARCH), aix-powerpc64)
FLAGS += -maix64
AR := ar -X 64 rc
endif

ifeq ($(TARGET_ARCH), sparc)
FLAGS += -m32 -mcpu=v8 -Wa,-xarch=v8plusa
endif

ifeq ($(TARGET_ARCH), x86)
FLAGS += -m32
ifeq (3, $(firstword $(sort $(GCC_MAJOR_VERSION) 3)))
OPTFLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
OPTFLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
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

# These flags can be overridden by the user 
CPPFLAGS :=
CFLAGS := 

INCFLAGS := -I. -Iplatform
OPTCFLAGS := $(CFLAGS) $(INCFLAGS) $(CPPFLAGS) $(FLAGS) $(OPTFLAGS)
DEBUGCFLAGS := $(CFLAGS) $(INCFLAGS) $(CPPFLAGS) $(FLAGS) -DASSERT=1 $(DEBUGFLAGS)
PICCFLAGS := $(CFLAGS) $(INCFLAGS) $(CPPFLAGS) $(FLAGS) $(OPTFLAGS) $(PICFLAGS)
GCOPTCFLAGS = $(GCOPTFLAGS)
GCDEBUGCFLAGS = $(GCDEBUGFLAGS)
GCPICCFLAGS = $(GCOPTFLAGS) $(GCPICFLAGS)
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
WARNCFLAGS += -Wbad-function-cast -Wcast-qual
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
PICWARNCFLAGS := $(WARNCFLAGS)  -Wdisabled-optimization $(OPTWARNFLAGS)


GDTOACFILES :=									\
	dmisc.c    g__fmt.c   misc.c     strtoIdd.c  strtopdd.c  strtorf.c	\
	dtoa.c     gmisc.c    smisc.c    strtoIf.c   strtopf.c   strtorQ.c	\
	g_ddfmt.c  g_Qfmt.c   strtod.c   strtoIg.c   strtopQ.c   strtorx.c	\
	g_dfmt.c   g_xfmt.c   strtodg.c  strtoIQ.c   strtopx.c   strtorxL.c	\
	gdtoa.c    g_xLfmt.c  strtodI.c  strtoIx.c   strtopxL.c  sum.c		\
	gethex.c   hd_init.c  strtof.c   strtoIxL.c  strtord.c   ulp.c		\
	g_ffmt.c   hexnan.c   strtoId.c  strtopd.c   strtordd.c
GDTOACFILES := $(patsubst %,gdtoa/%,$(GDTOACFILES))

UTILHFILES :=							\
	util.h							\
	$(shell find util -type f | grep '\.h$$')
UTILCFILES :=							\
	$(shell find util -type f | grep '\.c$$')

PLATFORMHFILES :=						\
	platform.h						\
	$(shell find platform -type f | grep '\.h$$')
PLATFORMCFILES :=						\
	$(shell find platform -type f | grep '\.c$$')

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


GDTOA_OBJS       := $(patsubst %.c,%.o,$(GDTOACFILES))
GDTOA_DEBUG_OBJS := $(patsubst %.c,%-gdb.o,$(GDTOACFILES))
GDTOA_PIC_OBJS   := $(patsubst %.c,%-pic.o,$(GDTOACFILES))


OBJS := 							\
	util.o							\
	platform.o						\
	platform/$(TARGET_OS).o					\
	gc.o

OMIT_BYTECODE := no
ifeq ($(OMIT_BYTECODE), yes)
else
  OBJS += bytecode/interpret.o
endif

ifeq ($(COMPILE_FAST), yes)
  OBJS += basis.o
else
  OBJS += 							\
	$(foreach f, $(basename $(BASISCFILES)), $(f).o)
endif

DEBUG_OBJS := $(patsubst %.o,%-gdb.o,$(OBJS))
PIC_OBJS   := $(patsubst %.o,%-pic.o,$(OBJS))


ALL := libgdtoa.a libgdtoa-gdb.a libgdtoa-pic.a \
       libmlton.a libmlton-gdb.a libmlton-pic.a
ALL += gen/c-types.sml gen/basis-ffi.sml gen/sizes
ifeq ($(OMIT_BYTECODE), yes)
else
  ALL += bytecode/opcodes
endif

all: $(ALL)


gdtoa/arithchk.c: gdtoa.tgz gdtoa-patch gdtoa-patch.internal gdtoa-patch.mlton
	gzip -dc gdtoa.tgz | tar xf -
	patch -s -p0 <gdtoa-patch
	patch -s -p0 <gdtoa-patch.internal
	patch -s -p0 <gdtoa-patch.mlton

$(GDTOACFILES): gdtoa/arithchk.c
	@touch $@

gdtoa/arithchk.out: gdtoa/arithchk.c
	cd gdtoa && $(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -w -O1 -o arithchk.out arithchk.c

gdtoa/arith.h: gdtoa/arithchk.out
	cd gdtoa && ./arithchk.out >arith.h

gdtoa/%-pic.o: gdtoa/%.c gdtoa/arith.h
	$(CC) $(PICCFLAGS) $(PICWARNCFLAGS) -w -DINFNAN_CHECK -c -o $@ $<
gdtoa/%-gdb.o: gdtoa/%.c gdtoa/arith.h
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -w -DINFNAN_CHECK -c -o $@ $<
gdtoa/%.o:     gdtoa/%.c gdtoa/arith.h
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -w -DINFNAN_CHECK -c -o $@ $<

libgdtoa.a:     $(GDTOA_OBJS)
libgdtoa-gdb.a: $(GDTOA_DEBUG_OBJS)
libgdtoa-pic.a: $(GDTOA_PIC_OBJS)


util-pic.o: util.c $(UTILCFILES) cenv.h $(UTILHFILES)
	$(CC) $(PICCFLAGS) $(PICWARNCFLAGS) -c -o $@ $<
util-gdb.o: util.c $(UTILCFILES) cenv.h $(UTILHFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<
util.o:     util.c $(UTILCFILES) cenv.h $(UTILHFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

c-types.h: gen/c-types.h
	cp $< $@
ml-types.h: gen/ml-types.h
	cp $< $@
gen/c-types.h gen/c-types.sml gen/ml-types.h: gen/gen-types.stamp
	@touch $@
gen/gen-types.stamp: gen/gen-types.c util.h util.o
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o gen/gen-types gen/gen-types.c util.o
	rm -f gen/c-types.h gen/c-types.sml gen/ml-types.h gen/gen-types.stamp
	cd gen && ./gen-types
	rm -f gen/gen-types$(EXE) gen/gen-types
	touch $@

basis-ffi.h: gen/basis-ffi.h
	cp $< $@
gen/basis-ffi.h gen/basis-ffi.sml: gen/gen-basis-ffi.stamp
	@touch $@
gen/gen-basis-ffi.stamp: gen/gen-basis-ffi.sml gen/basis-ffi.def
	mlton -output gen/gen-basis-ffi gen/gen-basis-ffi.sml
	rm -f gen/basis-ffi.h gen/basis-ffi.sml gen/gen-basis-ffi.stamp
	cd gen && ./gen-basis-ffi
	rm -f gen/gen-basis-ffi
	touch $@

gen/sizes: gen/gen-sizes.stamp
	@touch $@
gen/gen-sizes.stamp: gen/gen-sizes.c libmlton.a $(HFILES)
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -I. -o gen/gen-sizes gen/gen-sizes.c -L. -lmlton
	rm -f gen/sizes
	cd gen && ./gen-sizes
	rm -f gen/gen-sizes$(EXE) gen/gen-sizes
	touch $@

platform/$(TARGET_OS)-pic.o: $(PLATFORMCFILES)
platform/$(TARGET_OS)-gdb.o: $(PLATFORMCFILES)
platform/$(TARGET_OS).o: $(PLATFORMCFILES)

gc-pic.o: gc.c $(GCCFILES) $(HFILES)
	$(CC) $(PICCFLAGS) $(GCPICCFLAGS) $(PICWARNCFLAGS) -c -o $@ $<
gc-gdb.o: gc.c $(GCCFILES) $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(GCDEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<
gc.o: gc.c $(GCCFILES) $(HFILES)
	$(CC) $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

## Needs -Wno-float-equal for Real<N>_equal, included via "c-chunk.h".
bytecode/interpret-pic.o: bytecode/interpret.c $(HFILES) $(BYTECODEHFILES)
	$(CC) -I../include $(PICCFLAGS) $(GCPICCFLAGS) $(PICWARNCFLAGS) -Wno-float-equal -c -o $@ $<
bytecode/interpret-gdb.o: bytecode/interpret.c $(HFILES) $(BYTECODEHFILES)
	$(CC) -I../include $(DEBUGCFLAGS) $(GCDEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -c -o $@ $<
bytecode/interpret.o: bytecode/interpret.c $(HFILES) $(BYTECODEHFILES)
	$(CC) -I../include $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -c -o $@ $<

bytecode/opcodes: bytecode/print-opcodes
	@touch $@
bytecode/print-opcodes: bytecode/print-opcodes.c bytecode/opcode.h $(HFILES)
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o bytecode/print-opcodes bytecode/print-opcodes.c
	rm -f bytecode/opcodes
	cd bytecode && ./print-opcodes > opcodes

basis.c: $(BASISCFILES)
	rm -f basis.c
	cat $(BASISCFILES) >> basis.c

## Needs -Wno-float-equal for Real<N>_equal;
## needs -Wno-format-nonliteral for Date_strfTime;
## needs -Wno-redundant-decls for 'extern struct GC_state gcState'.
basis-pic.o: basis.c $(BASISCFILES) $(HFILES)
	$(CC) -Ibasis -Ibasis/Word -Ibasis/Real $(PICCFLAGS) $(PICWARNCFLAGS) -Wno-float-equal -Wno-format-nonliteral -Wno-redundant-decls -c -o $@ $<
basis-gdb.o: basis.c $(BASISCFILES) $(HFILES)
	$(CC) -Ibasis -Ibasis/Word -Ibasis/Real $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -Wno-format-nonliteral -Wno-redundant-decls -c -o $@ $<
basis.o: basis.c $(BASISCFILES) $(HFILES)
	$(CC) -Ibasis -Ibasis/Word -Ibasis/Real $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -Wno-format-nonliteral -Wno-redundant-decls -c -o $@ $<
## Needs -Wno-float-equal for Real<N>_equal.
basis/Real/Real-pic.o: basis/Real/Real.c $(HFILES)
	$(CC) $(PICCFLAGS) $(PICWARNCFLAGS) -Wno-float-equal -c -o $@ $<
basis/Real/Real-gdb.o: basis/Real/Real.c $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -c -o $@ $<
basis/Real/Real.o: basis/Real/Real.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -c -o $@ $<
## Needs -Wno-format-nonliteral for Date_strfTime.
basis/System/Date-pic.o: basis/System/Date.c $(HFILES)
	$(CC) $(PICCFLAGS) $(PICWARNCFLAGS) -Wno-format-nonliteral -c -o $@ $<
basis/System/Date-gdb.o: basis/System/Date.c $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-format-nonliteral -c -o $@ $<
basis/System/Date.o: basis/System/Date.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-format-nonliteral -c -o $@ $<

libmlton.a: $(OBJS)
libmlton-gdb.a: $(DEBUG_OBJS)
libmlton-pic.a: $(PIC_OBJS)


%-pic.o: %.c $(HFILES)
	$(CC) $(PICCFLAGS) $(PICWARNCFLAGS) -c -o $@ $<
%-gdb.o: %.c $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<
%.o: %.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

%.a:
	rm -f $@
	$(AR) $@ $^
	$(RANLIB) $@


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


.PHONY: clean
clean:
	../bin/clean


.PHONY: rebuild-gdtoa-patch
rebuild-gdtoa-patch:
	cd gdtoa && $(MAKE) clean && rm -f *~ *.orig
	mv gdtoa gdtoa-new
	gzip -dc gdtoa.tgz | tar xf -
	diff -P -C 2 -r gdtoa gdtoa-new >gdtoa-patch || exit 0
	rm -rf gdtoa
	mv gdtoa-new gdtoa
