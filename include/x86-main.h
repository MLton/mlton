/* Copyright (C) 2000-2005 Henry Cejtin, Matthew Fluet, Suresh
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
Word32 c_stackP;
Word32 divTemp;
Word32 eq1Temp;
Word32 eq2Temp;
Word32 fileTemp;
Word32 fildTemp;
Word32 fpswTemp;
Word32 indexTemp;
Word32 intInfTemp;
char MLton_bug_msg[] = "cps machine";
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
Word32 statusTemp;
Word32 switchTemp;
Word32 threadTemp;
Word8 wordTemp1B;
Word8 wordTemp2B;
Word16 wordTemp1W;
Word16 wordTemp2W;
Word32 wordTemp1L;
Word32 wordTemp2L;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
#endif

#if (defined (__CYGWIN__) || defined (__MSVCRT__))
#define ReturnToC "_Thread_returnToC"
#elif (defined (__FreeBSD__) || defined (__linux__) || defined (__NetBSD__) || defined (__OpenBSD__) || defined (__sun__))
#define ReturnToC "Thread_returnToC"
#else
#error ReturnToC not defined
#endif

static Word32 returnAddressToFrameIndex (Word32 w) {
        return *((Word32*)(w - sizeof(Word32)));
}

#define Main(al, mg, mfs, mmc, pk, ps, ml, reserveEsp)                  \
void MLton_jumpToSML (pointer jump) {                                   \
        Word lc_stackP;                                                 \
                                                                        \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_jumpToSML(0x%08x) starting\n", (uint)jump); \
        lc_stackP = c_stackP;                                           \
        if (reserveEsp)                                                 \
                __asm__ __volatile__                                    \
                ("pusha\nmovl %%esp,%0\nmovl %1,%%ebp\nmovl %2,%%edi\njmp *%3\n.global "ReturnToC"\n"ReturnToC":\nmovl %0,%%esp\npopa" \
                : "=o" (c_stackP)                                       \
                : "o" (gcState.stackTop), "o" (gcState.frontier), "r" (jump) \
                );                                                      \
        else                                                            \
                __asm__ __volatile__                                    \
                ("pusha\nmovl %%esp,%0\nmovl %1,%%ebp\nmovl %2,%%esp\njmp *%3\n.global "ReturnToC"\n"ReturnToC":\nmovl %0,%%esp\npopa" \
                : "=o" (c_stackP)                                       \
                : "o" (gcState.stackTop), "o" (gcState.frontier), "r" (jump) \
                );                                                      \
        c_stackP = lc_stackP;                                           \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_jumpToSML(0x%08x) done\n", (uint)jump); \
        return;                                                         \
}                                                                       \
void MLton_callFromC () {                                               \
        pointer jump;                                                   \
        GC_state s;                                                     \
                                                                        \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        s = &gcState;                                                   \
        s->savedThread = s->currentThread;                              \
        s->canHandle += 3;                                              \
        /* Return to the C Handler thread. */                           \
        GC_switchToThread (s, s->callFromCHandler, 0);                  \
        jump = *(pointer*)(s->stackTop - WORD_SIZE);                    \
        MLton_jumpToSML(jump);                                          \
        GC_switchToThread (s, s->savedThread, 0);                       \
        s->savedThread = BOGUS_THREAD;                                  \
        if (DEBUG_X86CODEGEN)                                           \
                fprintf (stderr, "MLton_callFromC() done\n");           \
        return;                                                         \
}                                                                       \
int main (int argc, char **argv) {                                      \
        pointer jump;                                                   \
        extern pointer ml;                                              \
                                                                        \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.isOriginal) {                                       \
                real_Init();                                            \
                jump = (pointer)&ml;                                    \
        } else {                                                        \
                jump = *(pointer*)(gcState.stackTop - WORD_SIZE);       \
        }                                                               \
        MLton_jumpToSML(jump);                                          \
        return 1;                                                       \
}

#endif /* #ifndef _X86_MAIN_H_ */

