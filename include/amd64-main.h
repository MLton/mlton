/* Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _AMD64_MAIN_H_
#define _AMD64_MAIN_H_

#include "common-main.h"

/* Globals */
PRIVATE Word64 applyFFTempFun;
PRIVATE Word64 applyFFTempStackArg;
PRIVATE Word64 applyFFTempRegArg[6];
PRIVATE Real32 applyFFTempXmmsRegArgD[8];
PRIVATE Real64 applyFFTempXmmsRegArgS[8];
PRIVATE Word32 checkTemp;
PRIVATE Word64 cReturnTemp[16];
PRIVATE Pointer c_stackP;
PRIVATE Word64 fpcvtTemp;
PRIVATE Word32 fpeqTemp;
PRIVATE Word64 divTemp;
PRIVATE Word64 indexTemp;
PRIVATE Word64 raTemp1;
PRIVATE Word64 spill[32];
PRIVATE Word64 stackTopTemp;

#ifndef DEBUG_AMD64CODEGEN
#define DEBUG_AMD64CODEGEN FALSE
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
        if (DEBUG_AMD64CODEGEN)                                         \
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
        if (0 == s->atomicState && s->signalsInfo.signalIsPending)      \
                s->limit = 0;                                           \
        if (DEBUG_AMD64CODEGEN)                                         \
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

#endif /* #ifndef _AMD64_MAIN_H_ */
