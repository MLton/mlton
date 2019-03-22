/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
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
static void MLton_callFromC () {                                        \
        uintptr_t nextBlock;                                            \
        uintptr_t (*nextChunk)(uintptr_t);                              \
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
        nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        do {                                                            \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
        } while (nextBlock != (uintptr_t)-1);                           \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState                                         \
            && s->signalsInfo.signalIsPending)                          \
                s->limit = 0;                                           \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC done\n");             \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, ml)                         \
MLtonCallFromC                                                          \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        uintptr_t nextBlock;                                            \
        uintptr_t (*nextChunk)(uintptr_t);                              \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                nextBlock = ml;                                         \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextBlock = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        /* Trampoline */                                                \
        while (1) {                                                     \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
        }                                                               \
        return 1;                                                       \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, mc, ml)                  \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
        uintptr_t nextBlock;                                            \
        uintptr_t (*nextChunk)(uintptr_t);                              \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                nextBlock = ml;                                         \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextBlock = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        /* Trampoline */                                                \
        do {                                                            \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
        } while (nextBlock != (uintptr_t)-1);                           \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        uintptr_t nextBlock;                                            \
        uintptr_t (*nextChuck)(uintptr_t);                              \
        nextBlock = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        do {                                                            \
                nextChunk = nextChunks[nextBlock];                      \
                nextBlock = (*nextChunk)(nextBlock);                    \
        } while (nextBlock != (uintptr_t)-1);                           \
        GC_done(&gcState);                                              \
}

#endif /* #ifndef _C_MAIN_H */
