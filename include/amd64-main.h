/* Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _AMD64_MAIN_H_
#define _AMD64_MAIN_H_

#include "main.h"

/* Globals */
Word64 applyFFTempFun;
Word64 applyFFTempStackArg;
Word64 applyFFTempRegArg[6];
Real32 applyFFTempXmmsRegArgD[8];
Real64 applyFFTempXmmsRegArgS[8];
Word32 checkTemp;
Word64 cReturnTemp[16];
Pointer c_stackP;
Word64 fpcvtTemp;
Word32 fpeqTemp;
Word64 divTemp;
Word64 indexTemp;
Word64 raTemp1;
Word64 spill[32];
Word64 stackTopTemp;

#ifndef DEBUG_AMD64CODEGEN
#define DEBUG_AMD64CODEGEN FALSE
#endif

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(ra - sizeof(GC_frameIndex)));
}

#define Main(al, mg, mfs, mmc, pk, ps, ml)                              \
void MLton_jumpToSML (pointer jump);                                    \
void MLton_callFromC () {                                               \
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
        GC_switchToThread (s, s->callFromCHandlerThread, 0);            \
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
}                                                                       \
int main (int argc, char **argv) {                                      \
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

#endif /* #ifndef _AMD64_MAIN_H_ */
