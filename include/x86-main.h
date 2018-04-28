/* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _X86_MAIN_H_
#define _X86_MAIN_H_

#include "common-main.h"

/* Globals */
PRIVATE Word32 applyFFTempFun;
PRIVATE Word32 applyFFTempArg;
PRIVATE Word32 checkTemp;
PRIVATE Word32 cReturnTemp[16];
PRIVATE Pointer c_stackP;
PRIVATE Word32 divTemp;
PRIVATE Word32 fildTemp;
PRIVATE Word32 fpswTemp;
PRIVATE Word32 indexTemp;
PRIVATE Word32 raTemp1;
PRIVATE Real64 raTemp2;
PRIVATE Real64 realTemp1D;
PRIVATE Real64 realTemp2D;
PRIVATE Real64 realTemp3D;
PRIVATE Real32 realTemp1S;
PRIVATE Real32 realTemp2S;
PRIVATE Real32 realTemp3S;
PRIVATE Word32 spill[16];
PRIVATE Word32 stackTopTemp;
PRIVATE Word8 wordTemp1B;
PRIVATE Word16 wordTemp1W;
PRIVATE Word32 wordTemp1L;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
#endif

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(ra - sizeof(GC_frameIndex)));
}

#define MLtonCallFromC                                                  \
PRIVATE void MLton_jumpToSML (pointer jump);                            \
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
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
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
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
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
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        pointer jump;                                                   \
        jump = *(pointer*)(gcState.stackTop - GC_RETURNADDRESS_SIZE);   \
        MLton_jumpToSML(jump);                                          \
        GC_done(&gcState);                                              \
}

#endif /* #ifndef _X86_MAIN_H_ */
