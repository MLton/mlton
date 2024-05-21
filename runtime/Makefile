## Copyright (C) 2010-2013,2018,2019-2022 Matthew Fluet.
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

EXE :=

WASMTIME :=

MDVARIANTS := OPT DBG
PIVARIANTS := DPI NPI PIC PIE

OPTSUFFIX :=
DBGSUFFIX := -dbg
DPISUFFIX :=
NPISUFFIX := -npi
PICSUFFIX := -pic
PIESUFFIX := -pie

WITH_OPT_RUNTIME := true
WITH_DBG_RUNTIME := true
WITH_DPI_RUNTIME := true
WITH_NPI_RUNTIME := $(or $(findstring true,$(RELEASE) $(WITH_ALL_RUNTIME)),false)
WITH_PIC_RUNTIME := $(or $(findstring true,$(RELEASE) $(WITH_ALL_RUNTIME)),false)
WITH_PIE_RUNTIME := $(or $(findstring true,$(RELEASE) $(WITH_ALL_RUNTIME)),false)

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
DBGXCFLAGS := -g -DASSERT=1 -Wno-uninitialized -O0
DPIXCFLAGS :=
NPIXCFLAGS := -fno-pic -fno-pie
PICXCFLAGS := -fPIC
PIEXCFLAGS := -fPIE
XLDFLAGS :=

ifneq ($(WITH_GMP_INC_DIR),)
XCPPFLAGS += -I$(WITH_GMP_INC_DIR)
endif

ifneq ($(WITH_GMP_LIB_DIR),)
XLDFLAGS += -L$(WITH_GMP_LIB_DIR)
endif

XCPPFLAGS += -I.

# Win32&64 don't use PIC code, all other platforms do
ifeq ($(findstring $(TARGET_OS), mingw cygwin),$(TARGET_OS))
WITH_PIC_RUNTIME := false
WITH_PIE_RUNTIME := false
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

ifeq ($(TARGET_OS), wasi)
WASMTIME := wasmtime
XCPPFLAGS += -D_WASI_EMULATED_SIGNAL \
             -D_WASI_EMULATED_PROCESS_CLOCKS \
             -D_WASI_EMULATED_GETPID
XLDFLAGS += -lwasi-emulated-signal \
            -lwasi-emulated-process-clocks \
            -lwasi-emulated-getpid
endif

ifeq ($(TARGET_OS), mingw)
EXE := .exe
# GCC doesn't recognize the %I64 format specifier which means %ll on windows
XCFLAGS += -Wno-format -Wno-missing-format-attribute
endif

ifeq ($(TARGET_OS), solaris)
XCFLAGS += -funroll-all-loops
endif

### MK_FLAGS ###

## MK_FLAGS (SRC,PHASE[,KS])
define MK_FLAGS
$(strip \
$(XCCFLAGS) $($(1)_XCCFLAGS) $(CCFLAGS) \
$(XMFLAGS) $($(1)_XMFLAGS) $(MFLAGS) \
$(if $(findstring $(2),CPP C),$(CPPFLAGS) $(XCPPFLAGS) $($(1)_XCPPFLAGS)) \
$(if $(findstring $(2),C), \
$(CFLAGS) \
$(XCFLAGS) \
$(foreach K,$(3),$($(K)XCFLAGS)) \
$($(1)_XCFLAGS) \
$(foreach K,$(3),$($(1)_$(K)XCFLAGS))) \
$(if $(findstring $(2),LD),$(LDFLAGS) $(XLDFLAGS) $($(1)_XLDFLAGS)) \
)
endef

### Position-independent (or not) ###

ifeq ($(shell echo "__pie__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C,PIE) -P -E -),2)
HAVE_PIE := true
ifeq ($(shell echo "__pie__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C) -P -E -),2)
WITH_DEFAULT_PIE := true
else
WITH_DEFAULT_PIE := false
endif
else
HAVE_PIE := false
WITH_PIE_RUNTIME := false
WITH_DEFAULT_PIE := false
endif

ifeq ($(shell echo "__pic__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C,PIC) -P -E -),2)
HAVE_PIC := true
ifeq ($(WITH_DEFAULT_PIE),true)
WITH_DEFAULT_PIC := false
else ifeq ($(shell echo "__pic__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C) -P -E -),2)
WITH_DEFAULT_PIC := true
else
WITH_DEFAULT_PIC := false
endif
else
HAVE_PIC := false
WITH_PIC_RUNTIME := false
WITH_DEFAULT_PIC := false
endif

ifeq ($(shell echo "__pic__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C,NPI) -P -E -),__pic__)
HAVE_NPI := true
ifeq ($(shell echo "__pic__" | $(CROSS_PREFIX)$(CC) $(call MK_FLAGS,-,C) -P -E -),__pic__)
WITH_DEFAULT_NPI := true
else
WITH_DEFAULT_NPI := false
endif
else
HAVE_NPI := false
WITH_NPI_RUNTIME := false
WITH_DEFAULT_NPI := false
endif

### pattern rules ###

%.d: %.c
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,CPP) -MM -MG $(foreach MD,$(MDVARIANTS),$(foreach PI,$(PIVARIANTS),-MT $(subst .d,$($(MD)SUFFIX)$($(PI)SUFFIX).o,$@))) -MT $@ -MM -MG -MF $@ $<

## O_C_TEMPLATE (MD,PI)
define O_C_TEMPLATE
%$($(1)SUFFIX)$($(2)SUFFIX).o: %.c
	$$(CROSS_PREFIX)$$(CC) $$(call MK_FLAGS,$$<,C,$(1) $(2)) -c -o $$@ $$<
endef

$(foreach MD,$(MDVARIANTS),$(foreach PI,$(PIVARIANTS), \
$(eval $(call O_C_TEMPLATE,$(MD),$(PI)))))

%$(EXE): %.o
	$(CROSS_PREFIX)$(CC) $(call MK_FLAGS,$<,LD) -o $@ $^

%.a:
	$(RM) $@
	$(CROSS_PREFIX)$(AR) rc $@ $^
	$(CROSS_PREFIX)$(RANLIB) $@

define A_TEMPLATE
$(foreach MD,$(MDVARIANTS),$(foreach PI,$(PIVARIANTS), \
ifeq ($(and $(findstring DPI,$(PI)),$(findstring true,$(foreach PIX,$(PIVARIANTS),$(WITH_DEFAULT_$(PIX))))),true)
$(foreach PIX,$(PIVARIANTS), \
ifeq ($(WITH_DEFAULT_$(PIX)),true)
lib$(1)$($(MD)SUFFIX).a: lib$(1)$($(MD)SUFFIX)$($(PIX)SUFFIX).a
	$$(CP) $$< $$@
endif
)
else
lib$(1)$($(MD)SUFFIX)$($(PI)SUFFIX).a: $(patsubst %.o,%$($(MD)SUFFIX)$($(PI)SUFFIX).o,$($(2)_OBJS))
endif
))
endef

### all ###

LIBS := \
$(foreach MD,$(MDVARIANTS), \
$(if $(findstring true,$(WITH_$(MD)_RUNTIME)),\
$(foreach PI,$(PIVARIANTS), \
$(if $(findstring true,$(WITH_$(PI)_RUNTIME)),\
$(patsubst %.a,%$($(MD)SUFFIX)$($(PI)SUFFIX).a,libgdtoa.a libmlton.a)))))
ALL += $(LIBS) gen/constants gen/c-types.sml gen/basis-ffi.sml

.PHONY: all
all: $(ALL)

### c-types.h  ml-types.h  gen/c-types.sml ###

c-types.h: gen/c-types.h
	$(CP) $< $@
ml-types.h: gen/ml-types.h
	$(CP) $< $@

gen/ml-types.h: gen/gen-types$(EXE)
	$(WASMTIME) ./gen/gen-types$(EXE) ml-types.h > gen/ml-types.h
gen/c-types.h: gen/gen-types$(EXE)
	$(WASMTIME) ./gen/gen-types$(EXE) c-types.h > gen/c-types.h
gen/c-types.sml: gen/gen-types$(EXE)
	$(WASMTIME) ./gen/gen-types$(EXE) c-types.sml > gen/c-types.sml

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-types.d
endif

gen/gen-types$(EXE): util.o

### basis-ffi.h  gen/basis-ffi.sml ###

basis-ffi.h: gen/basis-ffi.h
	$(CP) $< $@

define BASIS_FFI_TEMPLATE
ifeq ($(shell $(CAT) gen/gen-basis-ffi.sml gen/basis-ffi.def gen/$(1) | $(SHA1DGST)),$(shell $(CAT) gen/$(1).chk))
gen/$(1): gen/gen-basis-ffi.sml gen/basis-ffi.def
	touch gen/$(1)
else
gen/$(1): gen/gen-basis-ffi$(EXE) gen/basis-ffi.def
	$(WASMTIME) ./gen/gen-basis-ffi$(EXE) $(1) < gen/basis-ffi.def > gen/$(1)
	$(CAT) gen/gen-basis-ffi.sml gen/basis-ffi.def gen/$(1) | $(SHA1DGST) > gen/$(1).chk
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

$(eval $(call A_TEMPLATE,gdtoa,GDTOA))

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
	$(WASMTIME) ./gdtoa/arithchk$(EXE) > gdtoa/arith.h

gdtoa/arithchk.c: gdtoa/README
	@touch $@

gdtoa/arithchk.c_XCFLAGS := -w -O1

ifneq ($(MAKECMDGOALS),clean)
-include gdtoa/arithchk.d
endif

gdtoa/gd_qnan.h: gdtoa/qnan$(EXE)
	$(WASMTIME) ./gdtoa/qnan$(EXE) > gdtoa/gd_qnan.h

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

$(eval $(call A_TEMPLATE,mlton,MLTON))

### gen/constants ###

gen/constants: gen/gen-constants$(EXE)
	$(WASMTIME) ./gen/gen-constants > gen/constants

ifneq ($(MAKECMDGOALS),clean)
-include gen/gen-constants.d
endif

gen/gen-constants$(EXE): libmlton.a

### bootstrap ###

ifeq (true,$(shell if [ -d bootstrap ]; then echo true; else echo false; fi))
BOOTSTRAP_CFILES := $(shell $(FIND) bootstrap -type f -name '*.c')

BOOTSTRAP_OBJS := $(patsubst %.c,%.o,$(BOOTSTRAP_CFILES))

bootstrap/$(MLTON_OUTPUT)$(EXE): $(BOOTSTRAP_OBJS) libmlton.a libgdtoa.a
	$(CC) $(call MK_FLAGS,-,LD) -L. -o $@ $(BOOTSTRAP_OBJS) -lmlton -lgdtoa -lgmp -lm

$(BOOTSTRAP_OBJS): bootstrap/%.o: bootstrap/%.c
	$(CC) $(call MK_FLAGS,-,C) -O1 -fno-strict-aliasing -foptimize-sibling-calls -w -I$(ROOT)/include -w -c -o $@ $<
endif

######

SHOW_VARS += TARGET TARGET_ARCH TARGET_OS
SHOW_VARS += WITH_OPT_RUNTIME WITH_DBG_RUNTIME WITH_DPI_RUNTIME WITH_NPI_RUNTIME WITH_PIC_RUNTIME WITH_PIE_RUNTIME
SHOW_VARS += XCCFLAGS XMFLAGS XCPPFLAGS XCFLAGS OPTXCFLAGS DBGXCFLAGS DPIXCFLAGS NPIXCFLAGS PICXCFLAGS PIEXCFLAGS XLDFLAGS
SHOW_VARS += HAVE_NPI WITH_DEFAULT_NPI HAVE_PIE WITH_DEFAULT_PIE HAVE_PIC WITH_DEFAULT_PIC
SHOW_VARS += LIBS

$(eval $(MK_COMMON_GOALS))
