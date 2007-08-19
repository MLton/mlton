/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "main.h"
#include "c-common.h"

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return (GC_frameIndex)ra;
}

#define Main(al, mg, mfs, mmc, pk, ps, mc, ml)                          \
/* Globals */                                                           \
uintptr_t nextFun;                                                      \
int returnToC;                                                          \
void MLton_callFromC () {                                               \
        struct cont cont;                                               \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        s->savedThread = s->currentThread;                              \
        s->atomicState += 3;                                            \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, s->callFromCHandlerThread, 0);            \
        nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);   \
        cont.nextChunk = nextChunks[nextFun];                           \
        returnToC = FALSE;                                              \
        do {                                                            \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
        } while (not returnToC);                                        \
        GC_switchToThread (s, s->savedThread, 0);                       \
        s->savedThread = BOGUS_OBJPTR;                                  \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC done\n");             \
}                                                                       \
int main (int argc, char **argv) {                                      \
        struct cont cont;                                               \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                PrepFarJump(mc, ml);                                    \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
                cont.nextChunk = nextChunks[nextFun];                   \
        }                                                               \
        /* Trampoline */                                                \
        while (1) {                                                     \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
        }                                                               \
}

#endif /* #ifndef _C_MAIN_H */
