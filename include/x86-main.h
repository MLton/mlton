/* Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _X86_MAIN_H_
#define _X86_MAIN_H_

#include "common-main.h"

/* Globals */
INTERNAL Word32 applyFFTemp;
INTERNAL Word32 applyFFTemp2;
INTERNAL Word32 checkTemp;
INTERNAL Word32 cReturnTemp[16];
INTERNAL Pointer c_stackP;
INTERNAL Word32 divTemp;
INTERNAL Word32 fildTemp;
INTERNAL Word32 fpswTemp;
INTERNAL Word32 indexTemp;
INTERNAL Word32 raTemp1;
INTERNAL Real64 raTemp2;
INTERNAL Real64 realTemp1D;
INTERNAL Real64 realTemp2D;
INTERNAL Real64 realTemp3D;
INTERNAL Real32 realTemp1S;
INTERNAL Real32 realTemp2S;
INTERNAL Real32 realTemp3S;
INTERNAL Word32 spill[16];
INTERNAL Word32 stackTopTemp;
INTERNAL Word8 wordTemp1B;
INTERNAL Word16 wordTemp1W;
INTERNAL Word32 wordTemp1L;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
#endif

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(ra - sizeof(GC_frameIndex)));
}

#define MLtonCallFromC                                                  \
void MLton_jumpToSML (pointer jump);                                    \
static void MLton_callFromC () {                                        \
        pointer jump;                                                   \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Return to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE);        \
        MLton_jumpToSML(jump);                                          \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState                                         \
            && s->signalsInfo.signalIsPending)                          \
                s->limit = 0;                                           \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_callFromC() done\n");           \
        return;                                                         \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, ml)                         \
MLtonCallFromC                                                          \
EXPORTED int MLton_main (int argc, char* argv[]) {                      \
        pointer jump;                                                   \
        extern pointer ml;                                              \
                                                                        \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                jump = (pointer)&ml;                                    \
        } else {                                                        \
                jump = *(pointer*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        MLton_jumpToSML(jump);                                          \
        return 1;                                                       \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, ml)                      \
MLtonCallFromC                                                          \
EXPORTED void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {              \
        pointer jump;                                                   \
        extern pointer ml;                                              \
                                                                        \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                jump = (pointer)&ml;                                    \
        } else {                                                        \
                jump = *(pointer*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        MLton_jumpToSML(jump);                                          \
}                                                                       \
EXPORTED void LIB_CLOSE(LIBNAME) () {                                   \
        pointer jump;                                                   \
        jump = *(pointer*)(gcState.stackTop - GC_RETURNADDRESS_SIZE);   \
        MLton_jumpToSML(jump);                                          \
        GC_done(&gcState);                                              \
}

#endif /* #ifndef _X86_MAIN_H_ */
