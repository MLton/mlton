/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _BYTECODE_MAIN_H_
#define _BYTECODE_MAIN_H_

#include "common-main.h"
#include "interpret.h"

#ifndef DEBUG_CODEGEN
#define DEBUG_CODEGEN FALSE
#endif

PRIVATE extern struct Bytecode MLton_bytecode;

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(MLton_bytecode.code + ra - sizeof(GC_frameIndex)));
}

#define MLtonCallFromC                                                  \
static void MLton_callFromC () {                                        \
        uintptr_t nextFun;                                              \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_CODEGEN)                                              \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);   \
        MLton_Bytecode_interpret (&MLton_bytecode, nextFun);            \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState                                         \
            && s->signalsInfo.signalIsPending)                          \
                s->limit = 0;                                           \
        if (DEBUG_CODEGEN)                                              \
                fprintf (stderr, "MLton_callFromC done\n");             \
}                                                                       \

#define MLtonMain(al, mg, mfs, mmc, pk, ps, ml)                         \
MLtonCallFromC                                                          \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        uintptr_t nextFun;                                              \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                nextFun = ml;                                           \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        MLton_Bytecode_interpret (&MLton_bytecode, nextFun);            \
        return 1;                                                       \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, ml)                      \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
        uintptr_t nextFun;                                              \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.amOriginal) {                                       \
                real_Init();                                            \
                nextFun = ml;                                           \
        } else {                                                        \
                /* Return to the saved world */                         \
                nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        MLton_Bytecode_interpret (&MLton_bytecode, nextFun);            \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        uintptr_t nextFun;                                              \
        nextFun = *(uintptr_t*)(gcState.stackTop - GC_RETURNADDRESS_SIZE); \
        MLton_Bytecode_interpret (&MLton_bytecode, nextFun);            \
        GC_done(&gcState);                                              \
}

#endif /* #ifndef _BYTECODE_MAIN_H */
