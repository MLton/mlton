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

WITH_DBG_RUNTIME := true
WITH_PIC_RUNTIME := true

# These flags can be overridden by the user
CCFLAGS :=
MFLAGS :=
CPPFLAGS :=
CFLAGS :=
LDFLAGS :=

XCCFLAGS := -std=gnu11
XMFLAGS :=
XCPPFLAGS :=
XCFLAGS := -fno-common -pedantic -Wall -Wextra
OPTXCFLAGS := -Wdisabled-optimization -O2
DBGXCFLAGS := -DASSERT=1 -Wno-uninitialized -O0 -g
PICXCFLAGS := -fPIC
XLDFLAGS :=

ifneq ($(WITH_GMP_INC_DIR),)
XCPPFLAGS += -I$(WITH_GMP_INC_DIR)
endif

XCPPFLAGS += -I.

# Win32&64 don't use PIC code, all other platforms do
ifeq ($(findstring $(TARGET_OS), mingw cygwin),$(TARGET_OS))
WITH_PIC_RUNTIME := false
endif

# Make mlton library symbols private (win32&64 use another technique)
ifeq ($(findstring $(TARGET_OS), mingw cygwin),)
XCFLAGS += -fvisibility=hidden
endif

ifeq ($(TARGET_ARCH), alpha)
XMFLAGS += -mieee -mbwx -mtune=ev6 -mfp-rounding-mode=d
endif

ifeq ($(TARGET_ARCH), amd64)
XMFLAGS += -m64
endif

ifeq ($(findstring $(TARGET_ARCH), hppa ia64 powerpc sparc),$(TARGET_ARCH))
ifeq (4.2, $(firstword $(sort $(GCC_VERSION) 4.2)))
# GMP headers contain C99 inline functions which generate warnings
# with a suggestion to use this flag to disable the warnings.
XCFLAGS += -fgnu89-inline
endif
endif

ifeq ($(TARGET_ARCH), ia64)
XMFLAGS += -mtune=itanium2
ifeq ($(TARGET_OS), hpux)
XMFLAGS += -mlp64
endif
endif

ifeq ($(TARGET_OS)-$(TARGET_ARCH), aix-powerpc64)
XMFLAGS += -maix64
AR := ar -X 64 rc
endif

ifeq ($(TARGET_ARCH), sparc)
XMFLAGS += -m32 -mcpu=v8
XCFLAGS := -Wa,-xarch=v8plusa
endif

ifeq ($(TARGET_ARCH), x86)
XMFLAGS += -m32
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

### pattern rules ###

## MK_FLAGS (SRC,PHASE[,KS])
define MK_FLAGS
$(strip \
$(XCCFLAGS) $($(1)_XCCFLAGS) $(CCFLAGS) \
$(XMFLAGS) $($(1)_XMFLAGS) $(MFLAGS) \
$(if $(findstring $(2),CPP C),$(XCPPFLAGS) $($(1)_XCPPFLAGS) $(CPPFLAGS)) \
$(if $(findstring $(2),C), \
$(XCFLAGS) \
$(foreach K,$(3),$($(K)XCFLAGS)) \
$($(1)_XCFLAGS) \
$(foreach K,$(3),$($(1)_$(K)XCFLAGS)) \
$(CFLAGS)) \
$(if $(findstring $(2),LD),$(XLDFLAGS) $($(1)_XLDFLAGS) $(LDFLAGS)) \
)
endef

%.d: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,CPP) -MM -MG -MT $(subst .d,.o,$@) -MT $(subst .d,-dbg.o,$@) -MT $(subst .d,-pic.o,$@) -MT $(subst .d,-dbg-pic.o,$@) -MT $@ -MM -MG -MF $@ $<

%.o: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,C,OPT) -c -o $@ $<
%-dbg.o: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,C,DBG) -c -o $@ $<
%-pic.o: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,C,OPT PIC) -c -o $@ $<
%-dbg-pic.o: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,C,DBG PIC) -c -o $@ $<

%$(EXE): %.o
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,LD) -o $@ $^

ifeq ($(shell echo "__pic__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C) -P -E -),2)
WITH_DEFAULT_PIC := true
else
WITH_DEFAULT_PIC := false
endif

ifeq ($(WITH_DEFAULT_PIC),true)
%-pic.a: %.a
	$(CP) $< $@
endif

%.a:
	$(RM) $@
	$(CROSS_PREFIX)$(AR) rc $@ $^
	$(CROSS_PREFIX)$(RANLIB) $@

### all ###

LIBS := libgdtoa.a libmlton.a
ifeq ($(WITH_DBG_RUNTIME),true)
LIBS += $(patsubst %.a,%-dbg.a,$(LIBS))
endif
ifeq ($(WITH_PIC_RUNTIME),true)
LIBS += $(patsubst %.a,%-pic.a,$(LIBS))
endif
ALL += $(LIBS) gen/rconsts gen/c-types.sml gen/basis-ffi.sml

.PHONY: all
all: $(ALL)

### c-types.h  ml-types.h  gen/c-types.sml ###

c-types.h: gen/c-types.h
	$(CP) $< $@
ml-types.h: gen/ml-types.h
	$(CP) $< $@

gen/ml-types.h: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) ml-types.h > gen/ml-types.h
gen/c-types.h: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) c-types.h > gen/c-types.h
gen/c-types.sml: gen/gen-types$(EXE)
	./gen/gen-types$(EXE) c-types.sml > gen/c-types.sml

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-types.d
endif

gen/gen-types$(EXE): util.o

### basis-ffi.h  gen/basis-ffi.sml ###

basis-ffi.h: gen/basis-ffi.h
	$(CP) $< $@

define BASIS_FFI_TEMPLATE
ifeq ($(shell cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/$(1) | $(SHASUM) -c -s gen/$(1).chk && echo true),true)
gen/$(1): gen/gen-basis-ffi.sml gen/basis-ffi.def
	touch gen/$(1)
else
gen/$(1): gen/gen-basis-ffi$(EXE) gen/basis-ffi.def
	./gen/gen-basis-ffi$(EXE) $(1) < gen/basis-ffi.def > gen/$(1)
	cat gen/gen-basis-ffi.sml gen/basis-ffi.def gen/$(1) | $(SHASUM) > gen/$(1).chk
endif
endef

$(eval $(call BASIS_FFI_TEMPLATE,basis-ffi.h))
$(eval $(call BASIS_FFI_TEMPLATE,basis-ffi.sml))
$(eval $(call BASIS_FFI_TEMPLATE,gen-basis-ffi-consts.c))

gen/gen-basis-ffi$(EXE): gen/gen-basis-ffi.sml
	$(RUN_MLTON) -output gen/gen-basis-ffi$(EXE) gen/gen-basis-ffi.sml

### libgdtoa ###

GDTOA_CFILES := \
	dmisc.c     g_ddfmt.c   g_ffmt_p.c  gdtoa.c     misc.c      strtoIf.c   strtodI.c   strtopdd.c  strtord.c   sum.c \
	dtoa.c      g_ddfmt_p.c g_xLfmt.c   gethex.c    smisc.c     strtoIg.c   strtodg.c   strtopf.c   strtordd.c  ulp.c \
	g_Qfmt.c    g_dfmt.c    g_xLfmt_p.c gmisc.c     strtoIQ.c   strtoIx.c   strtof.c    strtopx.c   strtorf.c \
	g_Qfmt_p.c  g_dfmt_p.c  g_xfmt.c    hd_init.c   strtoId.c   strtoIxL.c  strtopQ.c   strtopxL.c  strtorx.c \
	g__fmt.c    g_ffmt.c    g_xfmt_p.c  hexnan.c    strtoIdd.c  strtod.c    strtopd.c   strtorQ.c   strtorxL.c
GDTOA_CFILES := $(patsubst %,gdtoa/%,$(GDTOA_CFILES))

GDTOA_OBJS := $(patsubst %.c,%.o,$(GDTOA_CFILES))

$(foreach F,$(GDTOA_CFILES), $(eval $(F)_XCFLAGS := -w -DINFNAN_CHECK))

ifneq ($(MAKECMDGOALS),clean)
-include $(patsubst %.o,%.d,$(GDTOA_OBJS))
endif

libgdtoa.a:         $(GDTOA_OBJS)
libgdtoa-dbg.a:     $(patsubst %.o,%-dbg.o,$(GDTOA_OBJS))
ifeq ($(WITH_DEFAULT_PIC),false)
libgdtoa-pic.a:     $(patsubst %.o,%-pic.o,$(GDTOA_OBJS))
libgdtoa-dbg-pic.a: $(patsubst %.o,%-dbg-pic.o,$(GDTOA_OBJS))
endif

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

gdtoa/gd_qnan.h: gdtoa/qnan$(EXE)
	./gdtoa/qnan$(EXE) > gdtoa/gd_qnan.h

gdtoa/qnan.c: gdtoa/README
	@touch $@

gdtoa/qnan.c_XCFLAGS := -w -O1

ifneq ($(MAKECMDGOALS),clean)
-include gdtoa/qnan.d
endif

### libmlton ###

BASIS_CFILES := $(shell $(FIND) basis -type f -name '*.c')

MLTON_OBJS := gc.o platform.o platform/$(TARGET_OS).o util.o
MLTON_OBJS += $(patsubst %.c,%.o,$(BASIS_CFILES))

gc.c_XCFLAGS := -Wno-address-of-packed-member

ifneq ($(MAKECMDGOALS),clean)
-include $(patsubst %.o,%.d,$(MLTON_OBJS))
endif

libmlton.a:         $(MLTON_OBJS)
libmlton-dbg.a:     $(patsubst %.o,%-dbg.o,$(MLTON_OBJS))
ifeq ($(WITH_DEFAULT_PIC),false)
libmlton-pic.a:     $(patsubst %.o,%-pic.o,$(MLTON_OBJS))
libmlton-dbg-pic.a: $(patsubst %.o,%-dbg-pic.o,$(MLTON_OBJS))
endif

### gen/rconsts ###

gen/rconsts: gen/gen-rconsts$(EXE)
	./gen/gen-rconsts > gen/rconsts

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-rconsts.d
endif

gen/gen-rconsts$(EXE): libmlton.a

######

SHOW_VARS += TARGET TARGET_ARCH TARGET_OS GCC_VERSION
SHOW_VARS += XCCFLAGS XMFLAGS XCPPFLAGS XCFLAGS OPTXCFLAGS DBGXCFLAGS PICXCFLAGS XLDFLAGS
SHOW_VARS += LIBS

$(eval $(MK_SHOW_CONFIG))


.PHONY: clean
clean:
	../bin/clean
