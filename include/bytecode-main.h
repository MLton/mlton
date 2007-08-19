/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _BYTECODE_MAIN_H_
#define _BYTECODE_MAIN_H_

#include "main.h"
#include "interpret.h"

#ifndef DEBUG_CODEGEN
#define DEBUG_CODEGEN FALSE
#endif

struct Bytecode MLton_bytecode;

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(MLton_bytecode.code + ra - sizeof(GC_frameIndex)));
}

#define Main(al, mg, mfs, mmc, pk, ps, ml)                              \
void MLton_callFromC () {                                               \
        uintptr_t nextFun;                                              \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_CODEGEN)                                              \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        s->savedThread = s->currentThread;                              \
        s->atomicState += 3;                                            \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, s->callFromCHandlerThread, 0);            \
        nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE);   \
        MLton_Bytecode_interpret (&MLton_bytecode, nextFun);            \
        GC_switchToThread (s, s->savedThread, 0);                       \
        s->savedThread = BOGUS_OBJPTR;                                  \
        if (DEBUG_CODEGEN)                                              \
                fprintf (stderr, "MLton_callFromC done\n");             \
}                                                                       \
int main (int argc, char **argv) {                                      \
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
}

#endif /* #ifndef _BYTECODE_MAIN_H */
