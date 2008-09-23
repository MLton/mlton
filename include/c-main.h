/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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

#define Main(al, mg, mfs, mmc, pk, ps, gnr, mc, ml)                     \
/* Globals */                                                           \
C_Pthread_Key_t gcstate_key;                                            \
void MLton_callFromC (uint32_t ffiOp) {                                 \
        struct cont cont;                                               \
        GC_state s = pthread_getspecific (gcstate_key);                 \
                                                                        \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s->savedThread = s->currentThread;                              \
        s->atomicState += 3;                                            \
        s->ffiOp = ffiOp;                                               \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, s->callFromCHandlerThread, 0);            \
        cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        cont.nextChunk = nextChunks[cont.nextFun];                      \
        s->returnToC = FALSE;                                           \
        do {                                                            \
                cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
        } while (not s->returnToC);                                     \
        GC_switchToThread (s, s->savedThread, 0);                       \
        s->savedThread = BOGUS_OBJPTR;                                  \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC done\n");             \
}                                                                       \
                                                                        \
void run (void *arg) {                                                  \
        struct cont cont;                                               \
        GC_state s = (GC_state)arg;                                     \
        uint32_t num = Proc_processorNumber (s)                         \
                * s->controls->affinityStride                           \
                + s->controls->affinityBase;                            \
        set_cpu_affinity(num);                                          \
                                                                        \
        /* Save our state locally */                                    \
        pthread_setspecific (gcstate_key, s);                           \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                PrepFarJump(cont, mc, ml);                              \
        } else {                                                        \
                /* Return to the saved world */                         \
                cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
                cont.nextChunk = nextChunks[cont.nextFun];              \
        }                                                               \
        /* Check to see whether or not we are the first thread */       \
        if (Proc_amPrimary (s)) {                                       \
                /* Trampoline */                                        \
                while (1) {                                             \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                }                                                       \
        }                                                               \
        else {                                                          \
                Proc_waitForInitialization (s);                         \
                Parallel_run ();                                        \
        }                                                               \
}                                                                       \
int main (int argc, char **argv) {                                      \
        int procNo;                                                     \
        pthread_t *threads;                                             \
        {                                                               \
                struct GC_state s;                                      \
                /* Initialize with a generic state to read in @MLtons, etc */ \
                Initialize (s, al, mg, mfs, mmc, pk, ps, gnr);          \
                                                                        \
                threads = (pthread_t *) malloc ((s.numberOfProcs - 1) * sizeof (pthread_t)); \
                gcState = (GC_state) malloc (s.numberOfProcs * sizeof (struct GC_state)); \
                /* Create key */                                        \
                if (pthread_key_create(&gcstate_key, NULL)) {           \
                        fprintf (stderr, "pthread_key_create failed: %s\n", strerror (errno)); \
                        exit (1);                                       \
                }                                                       \
                /* Now copy initialization to the first processor state */      \
                memcpy (&gcState[0], &s, sizeof (struct GC_state));     \
                gcState[0].procStates = gcState;                        \
                GC_lateInit (&gcState[0]);                              \
        }                                                               \
        /* Fill in per-processor data structures */                     \
        for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) { \
                Duplicate (&gcState[procNo], &gcState[0]);              \
                gcState[procNo].procStates = gcState;                   \
        }                                                               \
        /* Now create the threads */                                    \
        for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) { \
                if (pthread_create (&threads[procNo - 1], NULL, &run, (void *)&gcState[procNo])) { \
                        fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
                        exit (1);                                       \
                }                                                       \
        }                                                               \
        run ((void *)&gcState[0]);                                      \
}

#endif /* #ifndef _C_MAIN_H */
