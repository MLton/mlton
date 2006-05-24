/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

#include <stdio.h>

#include "ml-types.h"
#include "c-types.h"
#include "c-common.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

extern struct cont (*nextChunks []) ();
extern int nextFun;
extern int returnToC;
extern struct GC_state gcState;

#define GCState ((Pointer)&gcState)
#define ExnStack *(Word32*)(GCState + ExnStackOffset)
#define FrontierMem *(Pointer*)(GCState + FrontierOffset)
#define Frontier frontier
#define StackBottom *(Pointer*)(GCState + StackBottomOffset)
#define StackTopMem *(Pointer*)(GCState + StackTopOffset)
#define StackTop stackTop

/* ------------------------------------------------- */
/*                      Memory                       */
/* ------------------------------------------------- */

#define C(ty, x) (*(ty*)(x))
#define G(ty, i) (global##ty [i])
#define GPNR(i) G(PointerNonRoot, i)
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define X(ty, b, i, s, o) (*(ty*)((b) + ((i) * (s)) + (o)))
#define S(ty, i) *(ty*)(StackTop + (i))

/* ------------------------------------------------- */
/*                       Tests                       */
/* ------------------------------------------------- */

#define IsInt(p) (0x3 & (int)(p))

#define BZ(x, l)                                                        \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: BZ(%d, %s)\n", \
                                        __FILE__, __LINE__, (x), #l);   \
                if (0 == (x)) goto l;                                   \
        } while (0)

#define BNZ(x, l)                                                       \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: BNZ(%d, %s)\n",        \
                                        __FILE__, __LINE__, (x), #l);   \
                if (x) goto l;                                          \
        } while (0)

#define FlushFrontier()                         \
        do {                                    \
                FrontierMem = Frontier;         \
        } while (0)

#define FlushStackTop()                         \
        do {                                    \
                StackTopMem = StackTop;         \
        } while (0)

#define CacheFrontier()                         \
        do {                                    \
                Frontier = FrontierMem;         \
        } while (0)

#define CacheStackTop()                         \
        do {                                    \
                StackTop = StackTopMem;         \
        } while (0)

/* ------------------------------------------------- */
/*                       Chunk                       */
/* ------------------------------------------------- */

#if (defined (__sun__) && defined (REGISTER_FRONTIER_STACKTOP))
#define Chunk(n)                                                \
        DeclareChunk(n) {                                       \
                struct cont cont;                               \
                register unsigned int frontier asm("g5");       \
                int l_nextFun = nextFun;                        \
                register unsigned int stackTop asm("g6");
#else
#define Chunk(n)                                \
        DeclareChunk(n) {                       \
                struct cont cont;               \
                Pointer frontier;               \
                int l_nextFun = nextFun;        \
                Pointer stackTop;
#endif

#define ChunkSwitch(n)                                                  \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: entering chunk %d  l_nextFun = %d\n",  \
                                        __FILE__, __LINE__, n, l_nextFun);      \
                CacheFrontier();                                        \
                CacheStackTop();                                        \
                while (1) {                                             \
                top:                                                    \
                switch (l_nextFun) {

#define EndChunk                                                        \
                default:                                                \
                        /* interchunk return */                         \
                        nextFun = l_nextFun;                            \
                        cont.nextChunk = (void*)nextChunks[nextFun];    \
                        leaveChunk:                                     \
                                FlushFrontier();                        \
                                FlushStackTop();                        \
                                return cont;                            \
                } /* end switch (l_nextFun) */                          \
                } /* end while (1) */                                   \
        } /* end chunk */

/* ------------------------------------------------- */
/*                Calling SML from C                 */
/* ------------------------------------------------- */

#define Thread_returnToC()                                              \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Thread_returnToC()\n", \
                                        __FILE__, __LINE__);            \
                returnToC = TRUE;                                       \
                return cont;                                            \
        } while (0)

/* ------------------------------------------------- */
/*                      farJump                      */
/* ------------------------------------------------- */

#define FarJump(n, l)                           \
        do {                                    \
                PrepFarJump(n, l);              \
                goto leaveChunk;                \
        } while (0)

/* ------------------------------------------------- */
/*                       Stack                       */
/* ------------------------------------------------- */

#define Push(bytes)                                                     \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Push (%d)\n",          \
                                        __FILE__, __LINE__, bytes);     \
                StackTop += (bytes);                                    \
        } while (0)

#define Return()                                                                \
        do {                                                                    \
                l_nextFun = *(Word32*)(StackTop - sizeof(Word32));              \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Return()  l_nextFun = %d\n",   \
                                        __FILE__, __LINE__, l_nextFun);         \
                goto top;                                                       \
        } while (0)

#define Raise()                                                                 \
        do {                                                                    \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Raise\n",                      \
                                        __FILE__, __LINE__);                    \
                StackTop = StackBottom + ExnStack;                              \
                Return();                                                       \
        } while (0)                                                             \

/* ------------------------------------------------- */
/*                       Primitives                  */
/* ------------------------------------------------- */

#ifndef MLTON_CODEGEN_STATIC_INLINE
#define MLTON_CODEGEN_STATIC_INLINE static inline
#endif
/* Declare inlined math functions, since <math.h> isn't included.
 */
#ifndef MLTON_CODEGEN_MATHFN
#define MLTON_CODEGEN_MATHFN(decl) decl
#endif
/* WordS<N>_quot and WordS<N>_rem can't be inlined with the C-codegen,
 * because the gcc optimizer sometimes produces incorrect results when
 * one of the arguments is a constant.  
 * However, we need the declarations for Word-check.h
 */
#ifndef MLTON_CODEGEN_WORDSQUOTREM
#define MLTON_CODEGEN_WORDSQUOTREM(decl, func) decl
#endif
// #include "basis-ffi.h"
#include "basis/coerce.h"
#include "basis/Real/Real-ops.h"
#include "basis/Real/Math-fns.h"
#include "basis/Word/Word-ops.h"
#include "basis/Word/Word-consts.h"
#include "basis/Word/Word-check.h"

/* ------------------------------------------------- */
/*                        Real                       */
/* ------------------------------------------------- */

typedef volatile union {
        Word32 tab[2];
        Real64 d;
} Real64Or2Word32s;

static inline Real64 Real64_fetch (Real64 *dp) {
        Real64Or2Word32s u;
        Word32 *p;

        p = (Word32*)dp;
        u.tab[0] = p[0];
        u.tab[1] = p[1];
        return u.d;
}

static inline void Real64_move (Real64 *dst, Real64 *src) {
        Word32 *pd;
        Word32 *ps;
        Word32 t;

        pd = (Word32*)dst;
        ps = (Word32*)src;
        t = ps[1];
        pd[0] = ps[0];
        pd[1] = t;              
}

static inline void Real64_store (Real64 *dp, Real64 d) {
        Real64Or2Word32s u;
        Word32 *p;

        p = (Word32*)dp;
        u.d = d;
        p[0] = u.tab[0];
        p[1] = u.tab[1];
}

/* ------------------------------------------------- */
/*                        Word                       */
/* ------------------------------------------------- */

#define WordS_addCheckCX(size, dst, cW, xW, l)                  \
  do {                                                          \
    WordS##size c = cW;                                         \
    WordS##size x = xW;                                         \
    WordS_addCheckBodyCX(size, c, x, goto l, dst = c + x);      \
  } while (0)
#define WordS8_addCheckCX(dst, c, x, l) WordS_addCheckCX(8, dst, c, x, l)
#define WordS16_addCheckCX(dst, c, x, l) WordS_addCheckCX(16, dst, c, x, l)
#define WordS32_addCheckCX(dst, c, x, l) WordS_addCheckCX(32, dst, c, x, l)
#define WordS64_addCheckCX(dst, c, x, l) WordS_addCheckCX(64, dst, c, x, l)

#define WordS8_addCheckXC(dst, x, c, l) WordS8_addCheckCX(dst, c, x, l)
#define WordS16_addCheckXC(dst, x, c, l) WordS16_addCheckCX(dst, c, x, l)
#define WordS32_addCheckXC(dst, x, c, l) WordS32_addCheckCX(dst, c, x, l)
#define WordS64_addCheckXC(dst, x, c, l) WordS64_addCheckCX(dst, c, x, l)

#define WordS8_addCheck WordS8_addCheckXC
#define WordS16_addCheck WordS16_addCheckXC
#define WordS32_addCheck WordS32_addCheckXC
#define WordS64_addCheck WordS64_addCheckXC


#define WordU_addCheckCX(size, dst, cW, xW, l)                  \
  do {                                                          \
    WordU##size c = cW;                                         \
    WordU##size x = xW;                                         \
    WordU_addCheckBodyCX(size, c, x, goto l, dst = c + x);      \
  } while (0)
#define WordU8_addCheckCX(dst, c, x, l) WordU_addCheckCX(8, dst, c, x, l)
#define WordU16_addCheckCX(dst, c, x, l) WordU_addCheckCX(16, dst, c, x, l)
#define WordU32_addCheckCX(dst, c, x, l) WordU_addCheckCX(32, dst, c, x, l)
#define WordU64_addCheckCX(dst, c, x, l) WordU_addCheckCX(64, dst, c, x, l)

#define WordU8_addCheckXC(dst, x, c, l) WordU8_addCheckCX(dst, c, x, l)
#define WordU16_addCheckXC(dst, x, c, l) WordU16_addCheckCX(dst, c, x, l)
#define WordU32_addCheckXC(dst, x, c, l) WordU32_addCheckCX(dst, c, x, l)
#define WordU64_addCheckXC(dst, x, c, l) WordU64_addCheckCX(dst, c, x, l)

#define WordU8_addCheck WordU8_addCheckXC
#define WordU16_addCheck WordU16_addCheckXC
#define WordU32_addCheck WordU32_addCheckXC
#define WordU64_addCheck WordU64_addCheckXC


#define WordS_negCheck(size, dst, xW, l)                \
  do {                                                  \
    WordS##size x = xW;                                 \
    WordS_negCheckBody(size, x, goto l, dst = -x);      \
  } while (0)
#define Word8_negCheck(dst, x, l) WordS_negCheck(8, dst, x, l)
#define Word16_negCheck(dst, x, l) WordS_negCheck(16, dst, x, l)
#define Word32_negCheck(dst, x, l) WordS_negCheck(32, dst, x, l)
#define Word64_negCheck(dst, x, l) WordS_negCheck(64, dst, x, l)


#define WordS_subCheckCX(size, dst, cW, xW, l)                  \
  do {                                                          \
    WordS##size c = cW;                                         \
    WordS##size x = xW;                                         \
    WordS_subCheckBodyCX(size, c, x, goto l, dst = c - x);      \
  } while (0)
#define WordS8_subCheckCX(dst, c, x, l) WordS_subCheckCX(8, dst, c, x, l)
#define WordS16_subCheckCX(dst, c, x, l) WordS_subCheckCX(16, dst, c, x, l)
#define WordS32_subCheckCX(dst, c, x, l) WordS_subCheckCX(32, dst, c, x, l)
#define WordS64_subCheckCX(dst, c, x, l) WordS_subCheckCX(64, dst, c, x, l)

#define WordS_subCheckXC(size, dst, xW, cW, l)                  \
  do {                                                          \
    WordS##size x = xW;                                         \
    WordS##size c = cW;                                         \
    WordS_subCheckBodyXC(size, x, c, goto l, dst = x - c);      \
  } while (0)
#define WordS8_subCheckXC(dst, x, c, l) WordS_subCheckXC(8, dst, x, c, l)
#define WordS16_subCheckXC(dst, x, c, l) WordS_subCheckXC(16, dst, x, c, l)
#define WordS32_subCheckXC(dst, x, c, l) WordS_subCheckXC(32, dst, x, c, l)
#define WordS64_subCheckXC(dst, x, c, l) WordS_subCheckXC(64, dst, x, c, l)

#define WordS8_subCheck WordS8_subCheckXC
#define WordS16_subCheck WordS16_subCheckXC
#define WordS32_subCheck WordS32_subCheckXC
#define WordS64_subCheck WordS64_subCheckXC


#define WordS_mulCheck(size, dst, xW, yW, l)                    \
  do {                                                          \
    WordS##size x = xW;                                         \
    WordS##size y = yW;                                         \
    WordS_mulCheckBody(size, x, y, goto l, dst = x * y);        \
  } while (0)
#define WordS8_mulCheck(dst, x, y, l) WordS_mulCheck(8, dst, x, y, l)
#define WordS16_mulCheck(dst, x, y, l) WordS_mulCheck(16, dst, x, y, l)
#define WordS32_mulCheck(dst, x, y, l) WordS_mulCheck(32, dst, x, y, l)
#define WordS64_mulCheck(dst, x, y, l) WordS_mulCheck(64, dst, x, y, l)

#define WordU_mulCheck(size, dst, xW, yW, l)                    \
  do {                                                          \
    WordU##size x = xW;                                         \
    WordU##size y = yW;                                         \
    WordU_mulCheckBody(size, x, y, goto l, dst = x * y);        \
  } while (0)
#define WordU8_mulCheck(dst, x, y, l) WordU_mulCheck(8, dst, x, y, l)
#define WordU16_mulCheck(dst, x, y, l) WordU_mulCheck(16, dst, x, y, l)
#define WordU32_mulCheck(dst, x, y, l) WordU_mulCheck(32, dst, x, y, l)
#define WordU64_mulCheck(dst, x, y, l) WordU_mulCheck(64, dst, x, y, l)

#endif /* #ifndef _C_CHUNK_H_ */
