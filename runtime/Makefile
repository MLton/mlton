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
GCC_MAJOR_VERSION =						\
	$(shell gcc -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1/')
GCC_MINOR_VERSION =						\
	$(shell gcc -v 2>&1 | grep 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\2/')
GCC_VERSION = $(GCC_MAJOR_VERSION).$(GCC_MINOR_VERSION)

FLAGS = 
OPTFLAGS = -fomit-frame-pointer
GCOPTFLAGS = 
DEBUGFLAGS = 
WARNFLAGS =
OPTWARNFLAGS =
DEBUGWARNFLAGS = 

ifeq ($(TARGET_ARCH), x86)
ifeq ($(findstring $(GCC_MAJOR_VERSION), 3 4),$(GCC_MAJOR_VERSION))
FLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
FLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
endif
ifeq ($(findstring $(GCC_VERSION), 3.3 3.4 4.0),$(GCC_VERSION))
GCOPTFLAGS += --param max-inline-insns-single=1000
ifeq ($(findstring $(GCC_VERSION), 3.4 4.0),$(GCC_VERSION))
GCOPTFLAGS += --param inline-unit-growth=125
OPTWARNFLAGS += -Winline
endif
endif
ifeq ($(findstring $(GCC_VERSION), 4.1),$(GCC_VERSION))
OPTWARNFLAGS += -Winline
endif
DEBUGFLAGS += -gstabs+
endif

ifeq ($(TARGET_ARCH), amd64)
FLAGS += -m32
ifeq ($(findstring $(GCC_MAJOR_VERSION), 3),$(GCC_MAJOR_VERSION))
FLAGS += -mcpu=opteron
endif
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
FLAGS += -mtune=opteron
endif
ifeq ($(findstring $(GCC_VERSION), 3.4 4.0),$(GCC_VERSION))
GCOPTFLAGS += --param inline-unit-growth=75 --param max-inline-insns-single=1000
endif
DEBUGFLAGS += -gstabs+
OPTWARNFLAGS += -Winline
endif

ifeq ($(TARGET_ARCH), sparc)
FLAGS += -m32 -mcpu=v8 -Wa,-xarch=v8plusa
endif

ifeq ($(TARGET_OS), freebsd)
FLAGS += -I/usr/local/include
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
AR = ar rc
RANLIB = ranlib
else
AR = $(TARGET)-ar rc
RANLIB = $(TARGET)-ranlib
FLAGS += -b $(TARGET)
endif

CC = gcc -std=gnu99
CFLAGS = -I. -Iplatform $(FLAGS)
OPTCFLAGS = $(CFLAGS) -O2 $(OPTFLAGS)
DEBUGCFLAGS = $(CFLAGS) -g2 -O1 -DASSERT=1 $(DEBUGFLAGS)
GCOPTCFLAGS = $(GCOPTFLAGS)
WARNCFLAGS  =
WARNCFLAGS += -pedantic -Wall 
ifeq ($(findstring $(GCC_MAJOR_VERSION), 3),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -W
endif
ifeq ($(findstring $(GCC_MAJOR_VERSION), 4),$(GCC_MAJOR_VERSION))
WARNCFLAGS += -Wextra
endif
# -Wformat=2 implies -Wformat-nonliteral, which causes one spurious warning.
WARNCFLAGS += -Wformat=2
WARNCFLAGS += -Wno-format-nonliteral
# WARNCFLAGS += -Wswitch-default -Wswitch-enum
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
# WARNCFLAGS += -Wpacked -Wpadded
WARNCFLAGS += -Wredundant-decls 
WARNCFLAGS += -Wnested-externs 
WARNCFLAGS += -Wlong-long
# WARNCFLAGS += -Wunreachable-code
WARNCFLAGS += $(WARNFLAGS)
OPTWARNCFLAGS = $(WARNCFLAGS) -Wdisabled-optimization $(OPTWARNFLAGS)
DEBUGWARNCFLAGS = $(WARNCFLAGS) $(DEBUGWARNFLAGS)

# GCC doesn't recognize the %I64 format specifier which means %ll on windows
ifeq ($(TARGET_OS), mingw)
WARNCFLAGS += -Wno-format -Wno-missing-format-attribute
endif

UTILCFILES =							\
	$(shell find util -type f | grep '\.c$$')
UTILHFILES =							\
	$(shell find util -type f | grep '\.h$$')
UTILOFILES = $(foreach f, $(UTILCFILES), $(basename $(f)).o)

GCCFILES =							\
	$(shell find gc -type f | grep '\.c$$')
GCHFILES =							\
	$(shell find gc -type f | grep '\.h$$')

BASISHFILES =							\
	$(shell find basis -type f | grep '\.h$$')

BYTECFILES =							\
	$(shell find bytecode -type f | grep '\.c$$')
BYTEHFILES =							\
	$(shell find bytecode -type f | grep '\.h$$')

CFILES = 							\
	$(UTILCFILES)						\
	$(shell find basis -type f | grep '\.c$$' | grep -v Real/)	\
	gc.c							\
	bytecode/interpret.c					\
	platform.c

HFILES = 							\
	cenv.h                                                  \
	$(UTILHFILES)						\
	util.h							\
	$(GCHFILES)						\
	gc.h							\
	ml-types.h						\
	c-types.h						\
	basis-ffi.h						\
	platform.h						\
	platform/$(TARGET_OS).h					\
	platform/$(TARGET_ARCH).h				\
	$(BASISHFILES)

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

all:  libgdtoa.a libmlton.a libmlton-gdb.a 	\
	gen/c-types.sml gen/basis-ffi.sml	\
	bytecode/opcodes

libgdtoa.a: gdtoa/arith.h
	cd gdtoa && 					\
		$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS)	\
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
	rm -f runtime.c
	( echo "#define MLTON_GC_INTERNAL_TYPES"; 	\
	  echo "#define MLTON_GC_INTERNAL_FUNCS";	\
	  echo "#define MLTON_GC_INTERNAL_BASIS";	\
	  echo "#include \"platform.h\""; )		\
		> runtime.c
	cat $(CFILES) >> runtime.c


util/%-gdb.o: util/%.c util.h $(UTILHFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<

util/%.o: util/%.c util.h $(UTILHFILES)
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -c -o $@ $<

gen/c-types.h gen/c-types.sml gen/ml-types.h: gen/gen-types.c util.h $(UTILOFILES)
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o gen/gen-types gen/gen-types.c $(UTILOFILES)
	rm -f gen/c-types.h gen/c-types.sml gen/ml-types.h
	cd gen && ./gen-types
	rm -f gen/gen-types

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

bytecode/opcodes: bytecode/print-opcodes.c bytecode/opcode.h 
	$(CC) $(OPTCFLAGS) $(WARNCFLAGS) -o bytecode/print-opcodes bytecode/print-opcodes.c
	rm -f bytecode/opcodes
	cd bytecode && ./print-opcodes > opcodes
	rm -f bytecode/print-opcodes


gc-gdb.o: gc.c $(GCCFILES) $(HFILES) 
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<

gc.o: gc.c $(GCCFILES) $(HFILES) 
	$(CC) $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

bytecode/interpret-gdb.o: bytecode/interpret.c bytecode/opcode.h $(HFILES) 
	$(CC) -I../include $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -Wno-shadow -c -o $@ $<

bytecode/interpret.o: bytecode/interpret.c bytecode/opcode.h $(HFILES) 
	$(CC) -I../include $(OPTCFLAGS) $(GCOPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -Wno-shadow -c -o $@ $<

# It looks like we don't follow the C spec w.r.t. aliasing.  And gcc
# -O2 catches us on the code in Real/*.c where we treat a double as a
# chunk of two words.  Files that have been known to cause problems
# are class.c and gdtoa.c.  But there may be others.  So, we compile
# with -fno-strict-aliasing to prevent gcc from taking advantage of
# this aspect of the C spec.
basis/Real/%-gdb.o: basis/Real/%.c gdtoa/arith.h $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -Wno-float-equal -c -o $@ $<
basis/Real/%.o: basis/Real/%.c gdtoa/arith.h  $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -Wno-float-equal -O1 -fno-strict-aliasing -c -o $@ $<

%-gdb.o: %.c $(HFILES)
	$(CC) $(DEBUGCFLAGS) $(DEBUGWARNCFLAGS) -c -o $@ $<

%.o: %.c $(HFILES)
	$(CC) $(OPTCFLAGS) $(OPTWARNCFLAGS) -c -o $@ $<

%-gdb.o: %.S
	$(CC) $(DEBUGCFLAGS) -c -o $@ $<

%.o: %.S
	$(CC) $(OPTCFLAGS) -c -o $@ $<

.PHONY: flags
flags:
	echo TARGET = $(TARGET)
	echo TARGET_ARCH = $(TARGET_ARCH)
	echo TARGET_OS = $(TARGET_OS)
	echo GCC_MAJOR_VERSION = $(GCC_MAJOR_VERSION)
	echo GCC_MINOR_VERSION = $(GCC_MINOR_VERSION)
	echo GCC_VERSION = $(GCC_VERSION)
	echo FLAGS = $(FLAGS)
	echo OPTFLAGS = $(OPTFLAGS)
	echo GCOPTFLAGS = $(GCOPTFLAGS)
	echo DEBUGFLAGS = $(DEBUGFLAGS)
	echo WARNFLAGS = $(WARNFLAGS)
	echo OPTWARNFLAGS = $(OPTWARNFLAGS)
	echo DEBUGWARNFLAGS = $(DEBUGWARNFLAGS)


.PHONY: clean
clean:
	../bin/clean

.PHONY: gdtoa-patch
gdtoa-patch:
	cd gdtoa && $(MAKE) clean && rm -f *~
	mv gdtoa gdtoa-new
	gzip -dc gdtoa.tgz | tar xf -
	diff -P -C 2 -r gdtoa gdtoa-new >gdtoa-patch || exit 0
	rm -rf gdtoa
	mv gdtoa-new gdtoa
