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

PRIVATE GC_state MLton_gcState() {
  static struct GC_state gcState;
  return &gcState;
}

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return (GC_frameIndex)ra;
}

#define MLtonCallFromC()                                                \
static void MLton_callFromC (CPointer localOpArgsResPtr) {              \
        uintptr_t nextBlock;                                            \
        GC_state s = MLton_gcState();                                   \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s->callFromCOpArgsResPtr = localOpArgsResPtr;                   \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        do {                                                            \
                nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
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
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        uintptr_t nextBlock;                                            \
        GC_state s = MLton_gcState();                                   \
        Initialize (s, al, mg, mfs, mmc, pk, ps);                       \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                static_Init();                                          \
                nextBlock = ml;                                         \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        /* Trampoline */                                                \
        do {                                                            \
                nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
        } while (1);                                                    \
        return 1;                                                       \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, mc, ml)                  \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
        uintptr_t nextBlock;                                            \
        GC_state s = MLton_gcState();                                   \
        Initialize (s, al, mg, mfs, mmc, pk, ps);                       \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                static_Init();                                          \
                nextBlock = ml;                                         \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        /* Trampoline */                                                \
        do {                                                            \
                nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
        } while (nextBlock != (uintptr_t)-1);                           \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        uintptr_t nextBlock;                                            \
        GC_state s = MLton_gcState();                                   \
        nextBlock = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        do {                                                            \
                nextBlock = (*(nextChunks[nextBlock]))(s, s->stackTop, s->frontier, nextBlock); \
        } while (nextBlock != (uintptr_t)-1);                           \
        GC_done(s);                                                     \
}

#endif /* #ifndef _C_MAIN_H */
