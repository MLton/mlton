## Copyright (C) 2010-2013,2018,2019-2020 Matthew Fluet.
 # Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ..
include $(ROOT)/Makefile.config

######################################################################

ifeq ($(TARGET), self)
CROSS_PREFIX :=
else
CROSS_PREFIX := $(TARGET)-
endif

GCC_MAJOR_VERSION :=						\
	$(shell $(CC) -v 2>&1 | $(GREP) 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1/')
GCC_MINOR_VERSION :=						\
	$(shell $(CC) -v 2>&1 | $(GREP) 'gcc version' | 		\
		sed 's/.*gcc version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\2/')
GCC_VERSION := $(GCC_MAJOR_VERSION).$(GCC_MINOR_VERSION)

EXE :=

# These flags can be overridden by the user
CPPFLAGS :=
CFLAGS :=

WARNXCFLAGS :=
WARNXCFLAGS += -pedantic -Wall -Wextra

XCFLAGS := -std=gnu11 -fno-common $(WARNXCFLAGS)
OPTXCFLAGS := -Wdisabled-optimization -O2
DEBUGXCFLAGS := -DASSERT=1 -Wno-uninitialized -O0 -g
PICXCFLAGS :=

# Win32&64 don't use PIC code, all other platforms do
ifeq ($(findstring $(TARGET_OS), mingw cygwin),)
PICXCFLAGS += -fPIC
endif

# Make mlton library symbols private (win32&64 use another technique)
ifeq ($(findstring $(TARGET_OS), mingw cygwin),)
XCFLAGS += -fvisibility=hidden
endif

ifeq ($(TARGET_ARCH), alpha)
XCFLAGS += -mieee -mbwx -mtune=ev6 -mfp-rounding-mode=d
endif

ifeq ($(TARGET_ARCH), amd64)
XCFLAGS += -m64
endif

ifeq ($(findstring $(TARGET_ARCH), hppa ia64 powerpc sparc),$(TARGET_ARCH))
ifeq (4.2, $(firstword $(sort $(GCC_VERSION) 4.2)))
# GMP headers contain C99 inline functions which generate warnings
# with a suggestion to use this flag to disable the warnings.
XCFLAGS += -fgnu89-inline
endif
endif

ifeq ($(TARGET_ARCH), ia64)
XCFLAGS += -mtune=itanium2
ifeq ($(TARGET_OS), hpux)
XCFLAGS += -mlp64
endif
endif

ifeq ($(TARGET_OS)-$(TARGET_ARCH), aix-powerpc64)
XCFLAGS += -maix64
AR := ar -X 64 rc
endif

ifeq ($(TARGET_ARCH), sparc)
XCFLAGS += -m32 -mcpu=v8 -Wa,-xarch=v8plusa
endif

ifeq ($(TARGET_ARCH), x86)
XCFLAGS += -m32
endif

ifeq ($(TARGET_OS), cygwin)
EXE := .exe
endif

ifeq ($(TARGET_OS), mingw)
EXE := .exe
# GCC doesn't recognize the %I64 format specifier which means %ll on windows
XCFLAGS += -Wno-format -Wno-missing-format-attribute
endif

ifeq ($(TARGET_OS), solaris)
XCFLAGS += -funroll-all-loops
endif

ifneq ($(WITH_GMP_INC_DIR),)
XCFLAGS += -I$(WITH_GMP_INC_DIR)
endif

XCFLAGS += -I. -Iplatform
OPTCFLAGS := $(CFLAGS) $(CPPFLAGS) $(XCFLAGS) $(OPTXCFLAGS)
DEBUGCFLAGS := $(CFLAGS) $(CPPFLAGS) $(XCFLAGS) $(DEBUGXCFLAGS)
PICCFLAGS := $(OPTCFLAGS) $(PICXCFLAGS)

ALL := libgdtoa.a libgdtoa-gdb.a libgdtoa-pic.a \
       libmlton.a libmlton-gdb.a libmlton-pic.a
ALL += gen/c-types.sml gen/basis-ffi.sml gen/sizes

.PHONY: all
all: $(ALL)

### pattern rules ###

%.d: %.c
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) $($(<)_XCFLAGS) -MM -MG -MT $(subst .d,.o,$@) -MT $@ -MF $@ $<
%-gdb.d: %.c
	$(CROSS_PREFIX)$(CC) $(DEBUGCFLAGS) $($(<)_XCFLAGS) -MM -MG -MT $(subst .d,.o,$@) -MT $@ -MF $@ $<
%-pic.d: %.c
	$(CROSS_PREFIX)$(CC) $(PICCFLAGS) $($(<)_XCFLAGS) -MM -MG -MT $(subst .d,.o,$@) -MT $@ -MF $@ $<

%.o:
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) $($(<)_XCFLAGS) -c -o $@ $<
%-gdb.o:
	$(CROSS_PREFIX)$(CC) $(DEBUGCFLAGS) $($(<)_XCFLAGS) -c -o $@ $<
%-pic.o:
	$(CROSS_PREFIX)$(CC) $(PICCFLAGS) $($(<)_XCFLAGS) -c -o $@ $<

%.a:
	rm -f $@
	$(CROSS_PREFIX)$(AR) rc $@ $^
	$(CROSS_PREFIX)$(RANLIB) $@

### c-types.h  ml-types.h  gen/c-types.sml ###

c-types.h: gen/c-types.h
	cp $< $@
ml-types.h: gen/ml-types.h
	cp $< $@

gen/ml-types.h: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) ml-types.h > gen/ml-types.h
gen/c-types.h: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) c-types.h > gen/c-types.h
gen/c-types.sml: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) c-types.sml > gen/c-types.sml

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-types.d
endif

gen/gen-types$(EXE): gen/gen-types.o util.o
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) -o gen/gen-types gen/gen-types.o util.o

### basis-ffi.h  gen/basis-ffi.sml ###

basis-ffi.h: gen/basis-ffi.h
	cp $< $@

ifeq ($(shell cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/basis-ffi.h | shasum -c -s gen/basis-ffi.h.chk && echo true),true)
gen/basis-ffi.h: gen/gen-basis-ffi.sml gen/basis-ffi.def
	touch gen/basis-ffi.h
else
gen/basis-ffi.h: gen/gen-basis-ffi$(EXE) gen/basis-ffi.def
	./gen/gen-basis-ffi$(EXE) basis-ffi.h < gen/basis-ffi.def > gen/basis-ffi.h
	cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/basis-ffi.h | shasum > gen/basis-ffi.h.chk
endif

ifeq ($(shell cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/basis-ffi.sml | shasum -c -s gen/basis-ffi.sml.chk && echo true),true)
gen/basis-ffi.sml: gen/gen-basis-ffi.sml gen/basis-ffi.def
	touch gen/basis-ffi.sml
else
gen/basis-ffi.sml: gen/gen-basis-ffi$(EXE) gen/basis-ffi.def
	./gen/gen-basis-ffi$(EXE) basis-ffi.sml < gen/basis-ffi.def > gen/basis-ffi.sml
	cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/basis-ffi.sml | shasum > gen/basis-ffi.sml.chk
endif

gen/gen-basis-ffi$(EXE): gen/gen-basis-ffi.sml
	$(RUN_MLTON) -output gen/gen-basis-ffi$(EXE) gen/gen-basis-ffi.sml

### libgdtoa ###

GDTOACFILES :=									\
	dmisc.c     g_ddfmt.c   g_ffmt_p.c  gdtoa.c     misc.c      strtoIf.c   strtodI.c   strtopdd.c  strtord.c   sum.c \
	dtoa.c      g_ddfmt_p.c g_xLfmt.c   gethex.c    smisc.c     strtoIg.c   strtodg.c   strtopf.c   strtordd.c  ulp.c \
	g_Qfmt.c    g_dfmt.c    g_xLfmt_p.c gmisc.c     strtoIQ.c   strtoIx.c   strtof.c    strtopx.c   strtorf.c \
	g_Qfmt_p.c  g_dfmt_p.c  g_xfmt.c    hd_init.c   strtoId.c   strtoIxL.c  strtopQ.c   strtopxL.c  strtorx.c \
	g__fmt.c    g_ffmt.c    g_xfmt_p.c  hexnan.c    strtoIdd.c  strtod.c    strtopd.c   strtorQ.c   strtorxL.c
GDTOACFILES := $(patsubst %,gdtoa/%,$(GDTOACFILES))

GDTOA_OBJS       := $(patsubst %.c,%.o,$(GDTOACFILES))
GDTOA_DEBUG_OBJS := $(patsubst %.c,%-gdb.o,$(GDTOACFILES))
GDTOA_PIC_OBJS   := $(patsubst %.c,%-pic.o,$(GDTOACFILES))

$(foreach GDTOACFILE,$(GDTOACFILES), $(eval $(GDTOACFILE)_XCFLAGS := -w -DINFNAN_CHECK))

ifneq ($(MAKECMDGOALS),clean)
-include $(patsubst %.o,%.d,$(GDTOA_OBJS))
-include $(patsubst %.o,%.d,$(GDTOA_DEBUG_OBJS))
-include $(patsubst %.o,%.d,$(GDTOA_PIC_OBJS))
endif

libgdtoa.a:     $(GDTOA_OBJS)
libgdtoa-gdb.a: $(GDTOA_DEBUG_OBJS)
libgdtoa-pic.a: $(GDTOA_PIC_OBJS)


gdtoa/gdtoa.h $(GDTOACFILES): gdtoa/README
	@touch $@

gdtoa/README: gdtoa.tgz gdtoa.may_alias-unions.patch gdtoa.rename-public-fns.patch gdtoa.hide-private-fns.patch gdtoa.hide-public-fns.patch gdtoa.include-via-gdtoa.patch
	$(GZIP) -dc gdtoa.tgz | $(TAR) xf -
	$(PATCH) -s -d gdtoa -p1 <gdtoa.may_alias-unions.patch
	$(PATCH) -s -d gdtoa -p1 <gdtoa.rename-public-fns.patch
	$(PATCH) -s -d gdtoa -p1 <gdtoa.hide-private-fns.patch
	$(PATCH) -s -d gdtoa -p1 <gdtoa.hide-public-fns.patch
	$(PATCH) -s -d gdtoa -p1 <gdtoa.include-via-gdtoa.patch
	@touch $@


gdtoa/arith.h: gdtoa/arithchk$(EXE)
	./gdtoa/arithchk$(EXE) > gdtoa/arith.h

gdtoa/arithchk.c: gdtoa/README
	@touch $@

gdtoa/arithchk.c_XCFLAGS := -w -O1

ifneq ($(MAKECMDGOALS),clean)
-include gdtoa/arithchk.d
endif

gdtoa/arithchk$(EXE): gdtoa/arithchk.o
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) -o gdtoa/arithchk$(EXE) gdtoa/arithchk.o


gdtoa/gd_qnan.h: gdtoa/qnan$(EXE)
	./gdtoa/qnan$(EXE) > gdtoa/gd_qnan.h

gdtoa/qnan.c: gdtoa/README
	@touch $@

gdtoa/qnan.c_XCFLAGS := -w -O1

ifneq ($(MAKECMDGOALS),clean)
-include gdtoa/qnan.d
endif

gdtoa/qnan$(EXE): gdtoa/qnan.o
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) -o gdtoa/qnan$(EXE) gdtoa/qnan.o

### libmlton ###

BASISCFILES := $(shell $(FIND) basis -type f -name '*.c')

MLTON_OBJS := gc.o platform.o platform/$(TARGET_OS).o util.o
MLTON_OBJS += $(foreach f, $(basename $(BASISCFILES)), $(f).o)
MLTON_DEBUG_OBJS := $(patsubst %.o,%-gdb.o,$(MLTON_OBJS))
MLTON_PIC_OBJS   := $(patsubst %.o,%-pic.o,$(MLTON_OBJS))

gc.c_XCFLAGS := -Wno-unreachable-code
basis/Real/Real.c_XCFLAGS := -Wno-float-equal
basis/System/Date.c_XCFLAGS := -Wno-format-nonliteral

ifneq ($(MAKECMDGOALS),clean)
-include $(patsubst %.o,%.d,$(MLTON_OBJS))
-include $(patsubst %.o,%.d,$(MLTON_DEBUG_OBJS))
-include $(patsubst %.o,%.d,$(MLTON_PIC_OBJS))
endif

libmlton.a: $(MLTON_OBJS)
libmlton-gdb.a: $(MLTON_DEBUG_OBJS)
libmlton-pic.a: $(MLTON_PIC_OBJS)

### gen/sizes ###

gen/sizes: gen/gen-sizes$(EXE)
	./gen/gen-sizes > gen/sizes

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-sizes.d
endif

gen/gen-sizes$(EXE): gen/gen-sizes.o util.o
	$(CROSS_PREFIX)$(CC) $(OPTCFLAGS) -o gen/gen-sizes gen/gen-sizes.o util.o


######


.PHONY: flags
flags:
	@echo TARGET = $(TARGET)
	@echo TARGET_ARCH = $(TARGET_ARCH)
	@echo TARGET_OS = $(TARGET_OS)
	@echo GCC_MAJOR_VERSION = $(GCC_MAJOR_VERSION)
	@echo GCC_MINOR_VERSION = $(GCC_MINOR_VERSION)
	@echo GCC_VERSION = $(GCC_VERSION)
	@echo EXE = $(EXE)
	@echo XCFLAGS = $(XCFLAGS)
	@echo OPTXCFLAGS = $(OPTXCFLAGS)
	@echo DEBUGXCFLAGS = $(DEBUGXCFLAGS)
	@echo PICXCFLAGS = $(PICXCFLAGS)


.PHONY: clean
clean:
	../bin/clean
