/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "common-main.h"
#include "c-common.h"

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return (GC_frameIndex)ra;
}

#define MLtonCallFromC                                                  \
/* Globals */                                                           \
PRIVATE uintptr_t nextFun;                                              \
PRIVATE int returnToC;                                                  \
static void MLton_callFromC () {                                        \
        struct cont cont;                                               \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);   \
        cont.nextChunk = nextChunks[nextFun];                           \
        returnToC = FALSE;                                              \
        do {                                                            \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
        } while (not returnToC);                                        \
        returnToC = FALSE;                                              \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState                                         \
            && s->signalsInfo.signalIsPending)                          \
                s->limit = 0;                                           \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC done\n");             \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, mc, ml)                     \
MLtonCallFromC                                                          \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
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
        return 1;                                                       \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, mc, ml)                  \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
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
        returnToC = FALSE;                                              \
        do {                                                            \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
        } while (not returnToC);                                        \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        struct cont cont;                                               \
        nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        cont.nextChunk = nextChunks[nextFun];                           \
        returnToC = FALSE;                                              \
        do {                                                            \
                cont=(*(struct cont(*)(void))cont.nextChunk)();         \
        } while (not returnToC);                                        \
        GC_done(&gcState);                                              \
}

#endif /* #ifndef _C_MAIN_H */
