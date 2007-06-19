/* Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _X86_MAIN_H_
#define _X86_MAIN_H_

#include "main.h"

/* Globals */
Word32 applyFFTemp;
Word32 applyFFTemp2;
Word32 checkTemp;
Word32 cReturnTemp[16];
Pointer c_stackP;
Word32 divTemp;
Word32 fildTemp;
Word32 fpswTemp;
Word32 indexTemp;
Word32 raTemp1;
Real64 raTemp2;
Real64 realTemp1D;
Real64 realTemp2D;
Real64 realTemp3D;
Real32 realTemp1S;
Real32 realTemp2S;
Real32 realTemp3S;
Word32 spill[16];
Word32 stackTopTemp;
Word8 wordTemp1B;
Word16 wordTemp1W;
Word32 wordTemp1L;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
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
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        s->savedThread = s->currentThread;                              \
        s->atomicState += 3;                                            \
        /* Return to the C Handler thread. */                           \
        GC_switchToThread (s, s->callFromCHandlerThread, 0);            \
        jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE);        \
        MLton_jumpToSML(jump);                                          \
        GC_switchToThread (s, s->savedThread, 0);                       \
        s->savedThread = BOGUS_OBJPTR;                                  \
        if (DEBUG_X86CODEGEN)                                           \
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

#endif /* #ifndef _X86_MAIN_H_ */

