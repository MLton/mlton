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

#include "assert.h"
#include "c-common.h"
#include "ml-types.h"

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
                assert (StackBottom <= StackTop);                       \
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
/*                       Real                        */
/* ------------------------------------------------- */

#define unaryReal(f, g)                                         \
        Real64 g (Real64 x);                                    \
        static inline Real64 Real64_##f (Real64 x) {            \
                return g (x);                                   \
        }                                                       \
        static inline Real32 Real32_##f (Real32 x) {            \
                return (Real32)(Real64_##f ((Real64)x));        \
        }
unaryReal(round, rint)
#undef unaryReal

#define binaryReal(f, g)                                                        \
        Real64 g (Real64 x, Real64 y);                                          \
        static inline Real64 Real64_Math_##f (Real64 x, Real64 y) {             \
                return g (x, y);                                                \
        }                                                                       \
        static inline Real32 Real32_Math_##f (Real32 x, Real32 y) {             \
                return (Real32)(Real64_Math_##f ((Real64)x, (Real64)y));        \
        }
binaryReal(atan2, atan2)
#undef binaryReal

#define unaryReal(f, g)                                         \
        Real64 g (Real64 x);                                    \
        static inline Real64 Real64_Math_##f (Real64 x) {       \
                return g (x);                                   \
        }                                                       \
        static inline Real32 Real32_Math_##f (Real32 x) {       \
                return (Real32)(Real64_Math_##f ((Real64)x));   \
        }
unaryReal(acos, acos)
unaryReal(asin, asin)
unaryReal(atan, atan)
unaryReal(cos, cos)
unaryReal(exp, exp)
unaryReal(ln, log)
unaryReal(log10, log10)
unaryReal(sin, sin)
unaryReal(sqrt, sqrt)
unaryReal(tan, tan)
#undef unaryReal

double ldexp (double x, int i);
static inline Real64 Real64_ldexp (Real64 x, Int32 i) {
        return ldexp (x, i);
}
static inline Real32 Real32_ldexp (Real32 x, Int32 i) {
        return (Real32)Real64_ldexp ((Real64)x, i);
}

#define binaryReal(name, op)                                            \
        static inline Real32 Real32_##name (Real32 x, Real32 y) {       \
                return x op y;                                          \
        }                                                               \
        static inline Real64 Real64_##name (Real64 x, Real64 y) {       \
                return x op y;                                          \
        }
binaryReal(add, +)
binaryReal(div, /)
binaryReal(mul, *)
binaryReal(sub, -)
#undef binaryReal

#define binaryReal(name, op)                                    \
        static inline Bool Real32_##name (Real32 x, Real32 y) { \
                return x op y;                                  \
        }                                                       \
        static inline Bool Real64_##name (Real64 x, Real64 y) { \
                return x op y;                                  \
        }
binaryReal(equal, ==)
binaryReal(le, <=)
binaryReal(lt, <)
#undef binaryReal

#define Real32_muladd(x, y, z) ((x) * (y) + (z))
#define Real32_mulsub(x, y, z) ((x) * (y) - (z))
#define Real64_muladd(x, y, z) ((x) * (y) + (z))
#define Real64_mulsub(x, y, z) ((x) * (y) - (z))
#define Real32_neg(x) (-(x))
#define Real64_neg(x) (-(x))

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

#define wordBinary(size, name, op)                              \
        static inline Word##size Word##size##_##name            \
                        (Word##size w1, Word##size w2) {        \
                return w1 op w2;                                \
        }
#define wordCmp(size, name, op)                                                 \
        static inline Bool Word##size##_##name                                  \
                        (Word##size w1, Word##size w2) {                        \
                Bool res = w1 op w2;                                            \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s = 0x%08x " #op " 0x%08x\n",        \
                                        res ? "true": "false",                  \
                                        (unsigned int)w1,                       \
                                        (unsigned int)w2);                      \
                return w1 op w2;                                                \
        }
#define wordShift(size, name, op)                       \
        static inline Word##size Word##size##_##name    \
                        (Word##size w1, Word32 w2) {    \
                return w1 op w2;                        \
        }
#define wordUnary(size, name, op)                                       \
        static inline Word##size Word##size##_##name (Word##size w) {   \
                return op w;                                            \
        }
#define wordOps(size)                                                           \
        wordBinary (size, add, +)                                               \
        wordBinary (size, andb, &)                                              \
        wordBinary (S##size, mul, *)                                            \
        wordBinary (U##size, mul, *)                                            \
        wordBinary (size, orb, |)                                               \
        wordBinary (U##size, quot, /)                                           \
        wordBinary (U##size, rem, %)                                            \
        wordBinary (size, sub, -)                                               \
        wordBinary (size, xorb, ^)                                              \
        wordCmp (size, equal, ==)                                               \
        wordCmp (S##size, lt, <)                                                \
        wordCmp (U##size, lt, <)                                                \
        wordShift (size, lshift, <<)                                            \
        wordShift (U##size, rshift, >>)                                         \
        wordUnary (size, neg, -)                                                \
        wordUnary (size, notb, ~)                                               \
        /* WordS_rshift isn't ANSI C, because ANSI doesn't guarantee sign       \
         * extension.  We use it anyway cause it always seems to work.          \
         */                                                                     \
        static inline Word##size WordS##size##_rshift (WordS##size w, Word32 s) { \
                return w >> s;                                                  \
        }                                                                       \
        static inline Word##size Word##size##_rol (Word##size w1, Word32 w2) {  \
                return (w1 >> (size - w2)) | (w1 << w2);                        \
        }                                                                       \
        static inline Word##size Word##size##_ror (Word##size w1, Word32 w2) {  \
                return (w1 >> w2) | (w1 << (size - w2));                        \
        }
wordOps(8)
wordOps(16)
wordOps(32)
wordOps(64)
#undef wordBinary 
#undef wordCmp
#undef wordOps
#undef wordShift
#undef wordUnary

#define coerce(f, t)                            \
        static inline t f##_to##t (f x) {       \
                return (t)x;                    \
        }
coerce (Real32, Real64)
coerce (Real32, WordS32)
coerce (Real64, Real32)
coerce (Real64, WordS32)
coerce (WordS16, Real32)
coerce (WordS16, Real64)
coerce (WordS16, Word32)
coerce (WordS16, Word64)
coerce (WordS32, Real32)
coerce (WordS32, Real64)
coerce (WordS32, Word64)
coerce (WordS8, Real32)
coerce (WordS8, Real64)
coerce (WordS8, Word16)
coerce (WordS8, Word32)
coerce (WordS8, Word64)
coerce (WordU16, Word32)
coerce (WordU16, Word64)
coerce (WordU16, Word8)
coerce (WordU32, Word16)
coerce (WordU32, Word64)
coerce (WordU32, Word8)
coerce (WordU64, Word16)
coerce (WordU64, Word32)
coerce (WordU64, Word8)
coerce (WordU8, Word16)
coerce (WordU8, Word32)
coerce (WordU8, Word64)
#undef coerce

#define WordS8_max (WordS8)0x7F
#define WordS8_min (WordS8)0x80
#define WordS16_max (WordS16)0x7FFF
#define WordS16_min (WordS16)0x8000
#define WordS32_max (WordS32)0x7FFFFFFF
#define WordS32_min (WordS32)0x80000000
#define WordS64_max (WordS64)0x7FFFFFFFFFFFFFFFll
#define WordS64_min (WordS64)0x8000000000000000ll
#define Word8_max (Word8)0xFF
#define Word16_max (Word16)0xFFFF
#define Word32_max (Word32)0xFFFFFFFF
#define Word64_max (Word64)0xFFFFFFFFFFFFFFFFull

#define WordS_addCheckXC(size, dst, xW, cW, l)          \
        do {                                            \
                WordS##size x = xW;                     \
                WordS##size c = cW;                     \
                dst = x + c;                            \
                if (c >= 0) {                           \
                        if (x > WordS##size##_max - c)  \
                                goto l;                 \
                } else if (x < WordS##size##_min - c)   \
                                goto l;                 \
        } while (0)
#define WordS8_addCheckXC(dst, x, c, l) WordS_addCheckXC(8, dst, x, c, l)
#define WordS16_addCheckXC(dst, x, c, l) WordS_addCheckXC(16, dst, x, c, l)
#define WordS32_addCheckXC(dst, x, c, l) WordS_addCheckXC(32, dst, x, c, l)
#define WordS64_addCheckXC(dst, x, c, l) WordS_addCheckXC(64, dst, x, c, l)

#define WordS8_addCheckCX(dst, c, x, l) WordS8_addCheckXC(dst, x, c, l)
#define WordS16_addCheckCX(dst, c, x, l) WordS16_addCheckXC(dst, x, c, l)
#define WordS32_addCheckCX(dst, c, x, l) WordS32_addCheckXC(dst, x, c, l)
#define WordS64_addCheckCX(dst, c, x, l) WordS64_addCheckXC(dst, x, c, l)

#define WordS8_addCheck(dst, x, y, l) WordS8_addCheckXC (dst, x, y, l)
#define WordS16_addCheck WordS16_addCheckXC
#define WordS32_addCheck WordS32_addCheckXC
#define WordS64_addCheck WordS64_addCheckXC

#define WordS_negCheck(size, dst, nW, l)        \
        do {                                    \
                WordS##size n = nW;             \
                dst = -n;                       \
                if (n == WordS##size##_min)     \
                        goto l;                 \
        } while (0)

#define Word8_negCheck(dst, n, l) WordS_negCheck(8, dst, n, l)
#define Word16_negCheck(dst, n, l) WordS_negCheck(16, dst, n, l)
#define Word32_negCheck(dst, n, l) WordS_negCheck(32, dst, n, l)
#define Word64_negCheck(dst, n, l) WordS_negCheck(64, dst, n, l)

#define WordS_subCheckCX(size, dst, cW, xW, l)          \
        do {                                            \
                WordS##size c = cW;                     \
                WordS##size x = xW;                     \
                dst = c - x;                            \
                if (c >= 0) {                           \
                        if (x < c - WordS##size##_max)  \
                                goto l;                 \
                } else if (x > c - WordS##size##_min)   \
                        goto l;                         \
        } while (0)
#define WordS8_subCheckCX(dst, c, x, l) WordS_subCheckCX(8, dst, c, x, l)
#define WordS16_subCheckCX(dst, c, x, l) WordS_subCheckCX(16, dst, c, x, l)
#define WordS32_subCheckCX(dst, c, x, l) WordS_subCheckCX(32, dst, c, x, l)
#define WordS64_subCheckCX(dst, c, x, l) WordS_subCheckCX(64, dst, c, x, l)

#define WordS_subCheckXC(size, dst, xW, cW, l)          \
        do {                                            \
                WordS##size c = cW;                     \
                WordS##size x = xW;                     \
                if (c <= 0) {                           \
                        if (x > WordS##size##_max + c)  \
                                goto l;                 \
                } else if (x < WordS##size##_min + c)   \
                        goto l;                         \
                dst = x - c;                            \
        } while (0)
#define WordS8_subCheckXC(dst, c, x, l) WordS_subCheckXC(8, dst, c, x, l)
#define WordS16_subCheckXC(dst, c, x, l) WordS_subCheckXC(16, dst, c, x, l)
#define WordS32_subCheckXC(dst, c, x, l) WordS_subCheckXC(32, dst, c, x, l)
#define WordS64_subCheckXC(dst, c, x, l) WordS_subCheckXC(64, dst, c, x, l)

#define WordS8_subCheck WordS8_subCheckXC
#define WordS16_subCheck WordS16_subCheckXC
#define WordS32_subCheck WordS32_subCheckXC
#define WordS64_subCheck WordS64_subCheckXC

#define WordU_addCheckXC(size, dst, x, c, l)    \
        do {                                    \
                dst = x + c;                    \
                if (x > Word##size##_max - c)   \
                        goto l;                 \
        } while (0)
#define WordU8_addCheckXC(dst, x, c, l) WordU_addCheckXC(8, dst, x, c, l)
#define WordU16_addCheckXC(dst, x, c, l) WordU_addCheckXC(16, dst, x, c, l)
#define WordU32_addCheckXC(dst, x, c, l) WordU_addCheckXC(32, dst, x, c, l)
#define WordU64_addCheckXC(dst, x, c, l) WordU_addCheckXC(64, dst, x, c, l)
#define WordU8_addCheckCX(dst, c, x, l) WordU_addCheckXC(8, dst, x, c, l)
#define WordU16_addCheckCX(dst, c, x, l) WordU_addCheckXC(16, dst, x, c, l)
#define WordU32_addCheckCX(dst, c, x, l) WordU_addCheckXC(32, dst, x, c, l)
#define WordU64_addCheckCX(dst, c, x, l) WordU_addCheckXC(64, dst, x, c, l)

#define WordU8_addCheck WordU8_addCheckXC
#define WordU16_addCheck WordU16_addCheckXC
#define WordU32_addCheck WordU32_addCheckXC
#define WordU64_addCheck WordU64_addCheckXC

#define mulOverflow(small, large)                                               \
        static inline Word##small Word##small##_##mulOverflow                   \
                        (Word##small x1, Word##small x2, Bool *overflow) {      \
                Word##large tmp;                                                \
                Word##small res;                                                \
                                                                                \
                tmp = (Word##large)x1 * x2;                                     \
                res = tmp;                                                      \
                *overflow = (tmp != res);                                       \
                return res;                                                     \
        }
mulOverflow(S8, S16)
mulOverflow(S16, S32)
mulOverflow(S32, S64)
mulOverflow(U8, U16)
mulOverflow(U16, U32)
mulOverflow(U32, U64)
#undef mulOverflow

#define check(dst, n1, n2, l, ty);                                              \
        do {                                                                    \
                Bool overflow;                                                  \
                ty tmp;                                                         \
                tmp = ty##_mulOverflow (n1, n2, &overflow);                     \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: " #ty "_mulOverflow (%d, %d) = %d\n",  \
                                        __FILE__, __LINE__,                     \
                                        (int)n1, (int)n2, (int)tmp);            \
                if (overflow) {                                                 \
                        if (DEBUG_CCODEGEN)                                     \
                                fprintf (stderr, "%s:%d: overflow\n",           \
                                                __FILE__, __LINE__);            \
                        goto l;                                                 \
                }                                                               \
                dst = tmp;                                                      \
        } while (0)

#define WordS8_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordS8)
#define WordS16_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordS16)
#define WordS32_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordS32)
#define WordU8_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordU8)
#define WordU16_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordU16)
#define WordU32_mulCheck(dst, n1, n2, l)  check (dst, n1, n2, l, WordU32)

#endif /* #ifndef _C_CHUNK_H_ */
