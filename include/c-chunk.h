#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

#include "my-lib.h"
#include "c-common.h"
#include "types.h"

#define WORD_SIZE 4

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

extern struct cont (*nextChunks []) ();
extern Int nextFun;
extern Int returnToC;
extern struct GC_state gcState;

#define GCState ((Pointer)&gcState)
#define ExnStack *(Word*)(GCState + ExnStackOffset)
#define FrontierMem *(Word*)(GCState + FrontierOffset)
#define Frontier frontier
#define StackBottom *(Word*)(GCState + StackBottomOffset)
#define StackTopMem *(Word*)(GCState + StackTopOffset)
#define StackTop stackTop

/* ------------------------------------------------- */
/*                      Memory                       */
/* ------------------------------------------------- */

#define C(ty, x) (*(ty*)(x))
#define G(ty, i) (global##ty [i])
#define GPNR(i) G(PointerNonRoot, i)
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define X(ty, b, i) (*(ty*)((b) + ((i) * sizeof(ty))))
#define S(ty, i) *(ty*)(StackTop + (i))

/* ------------------------------------------------- */
/*                       Tests                       */
/* ------------------------------------------------- */

#define IsInt(p) (0x3 & (int)(p))

#define BZ(x, l)							\
	do {								\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: BZ(%d, %s)\n",	\
					__FILE__, __LINE__, (x), #l);	\
		if (0 == (x)) goto l;					\
	} while (0)

#define BNZ(x, l)							\
	do {								\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: BNZ(%d, %s)\n",	\
					__FILE__, __LINE__, (x), #l);	\
		if (x) goto l;						\
	} while (0)

#define FlushFrontier()				\
	do {					\
		FrontierMem = Frontier;		\
	} while (0)

#define FlushStackTop()				\
	do {					\
		StackTopMem = StackTop;		\
	} while (0)

#define CacheFrontier()				\
	do {					\
		Frontier = FrontierMem;		\
	} while (0)

#define CacheStackTop()				\
	do {					\
		StackTop = StackTopMem;		\
	} while (0)

/* ------------------------------------------------- */
/*                       Chunk                       */
/* ------------------------------------------------- */

#if (defined (__sun__) && defined (REGISTER_FRONTIER_STACKTOP))
#define Chunk(n)						\
	DeclareChunk(n) {					\
		struct cont cont;				\
		register unsigned int frontier asm("g5");	\
		int l_nextFun = nextFun;			\
		register unsigned int stackTop asm("g6");
#else
#define Chunk(n)				\
	DeclareChunk(n) {			\
		struct cont cont;		\
		Pointer frontier;		\
		int l_nextFun = nextFun;	\
		Pointer stackTop;
#endif

#define ChunkSwitch(n)							\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: entering chunk %d  l_nextFun = %d\n",	\
					__FILE__, __LINE__, n, l_nextFun);	\
		CacheFrontier();					\
		CacheStackTop();					\
		while (1) {						\
		top:							\
		switch (l_nextFun) {

#define EndChunk							\
		default:						\
			/* interchunk return */				\
			nextFun = l_nextFun;				\
			cont.nextChunk = (void*)nextChunks[nextFun];	\
			leaveChunk:					\
				FlushFrontier();			\
				FlushStackTop();			\
				return cont;				\
		} /* end switch (l_nextFun) */				\
		} /* end while (1) */					\
	} /* end chunk */

/* ------------------------------------------------- */
/*                Calling SML from C                 */
/* ------------------------------------------------- */

#define Thread_returnToC()						\
	do {								\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: Thread_returnToC()\n",	\
					__FILE__, __LINE__);		\
		returnToC = TRUE;					\
		return cont;						\
	} while (0)

/* ------------------------------------------------- */
/*                      farJump                      */
/* ------------------------------------------------- */

#define FarJump(n, l)	 			\
	do {					\
		PrepFarJump(n, l); 		\
		goto leaveChunk;		\
	} while (0)

/* ------------------------------------------------- */
/*                       Stack                       */
/* ------------------------------------------------- */

#define Push(bytes)							\
	do {								\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: Push (%d)\n",		\
					__FILE__, __LINE__, bytes);	\
		StackTop += (bytes);					\
		assert (StackBottom <= StackTop);			\
	} while (0)

#define Return()								\
	do {									\
		l_nextFun = *(Word*)(StackTop - WORD_SIZE);			\
		if (DEBUG_CCODEGEN)						\
			fprintf (stderr, "%s:%d: Return()  l_nextFun = %d\n",	\
					__FILE__, __LINE__, l_nextFun);		\
		goto top;							\
	} while (0)

#define Raise()									\
	do {									\
		if (DEBUG_CCODEGEN)						\
			fprintf (stderr, "%s:%d: Raise\n",			\
					__FILE__, __LINE__);			\
		StackTop = StackBottom + ExnStack;				\
		Return();							\
	} while (0)								\

#define DeclareProfileLabel(l)			\
	void l() __attribute__ ((weak))

#define ProfileLabel(l)				\
	__asm__ __volatile__ (#l ## ":" : : )

#define SmallIntInf(n) ((Pointer)(n))

#define Object(x, h)							\
	do {								\
		*(Word*)Frontier = (h);					\
		x = Frontier + WORD_SIZE;				\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%s:%d: 0x%x = Object(%d)\n",	\
					__FILE__, __LINE__, x, h);	\
	} while (0)

#define EndObject(bytes)			\
	do {					\
		Frontier += (bytes);		\
	} while (0)

/* ------------------------------------------------- */
/*                     Cpointer                      */
/* ------------------------------------------------- */

#define Cpointer_isNull(x) (NULL == (void*)(x))

/* ------------------------------------------------- */
/*                        Int                        */
/* ------------------------------------------------- */

/* The default is to use INT_TEST. */
#if (! defined (INT_NO_CHECK) && ! defined (INT_TEST))
#define INT_TEST
#endif

#if (defined (INT_NO_CHECK))
#define Int_addCheck(dst, n1, n2, l) dst = n1 + n2
#define Int_mulCheck(dst, n1, n2, l) dst = n1 * n2
#define Int_negCheck(dst, n, l) dst = -n
#define Int_subCheck(dst, n1, n2, l) dst = n1 - n2
#define Word32_addCheck(dst, n1, n2, l) dst = n1 + n2
#define Word32_mulCheck(dst, n1, n2, l) dst = n1 * n2
#define Int_addCheckCX Int_addCheck
#define Int_addCheckXC Int_addCheck
#define Int_subCheckCX Int_subCheck
#define Int_subCheckXC Int_subCheck
#define Word32_addCheckCX Word32_addCheck
#define Word32_addCheckXC Word32_addCheck
#endif

#if (defined (INT_TEST))

#define Int8_max (Int8)0x7F
#define	Int8_min (Int8)0x80
#define Int16_max (Int16)0x7FFF
#define Int16_min (Int16)0x8000
#define Int32_max (Int32)0x7FFFFFFF
#define Int32_min (Int32)0x80000000
//#define Int64_max (Int64)0x7FFFFFFFFFFFFFFF
//#define Int64_min (Int64)0x8000000000000000
#define Word8_max (Word8)0xFF
#define Word16_max (Word16)0xFFFF
#define Word32_max (Word32)0xFFFFFFFF
//#define Word64_max (Word64)0xFFFFFFFFFFFFFFFF

#define Int_addCheckXC(size, dst, x, c, l)		\
	do {						\
		if (c >= 0) {				\
			if (x > Int##size##_max - c)	\
				goto l;			\
		} else if (x < Int##size##_min - c)	\
				goto l;			\
		dst = x + c;				\
	} while (0)
#define Int8_addCheckXC(dst, x, c, l) Int_addCheckXC(8, dst, x, c, l)
#define Int16_addCheckXC(dst, x, c, l) Int_addCheckXC(16, dst, x, c, l)
#define Int32_addCheckXC(dst, x, c, l) Int_addCheckXC(32, dst, x, c, l)
//#define Int64_addCheckXC(dst, x, c, l) Int_addCheckXC(64, dst, x, c, l)

#define Int8_addCheckCX(dst, c, x, l) Int8_addCheckXC(dst, x, c, l)
#define Int16_addCheckCX(dst, c, x, l) Int16_addCheckXC(dst, x, c, l)
#define Int32_addCheckCX(dst, c, x, l) Int32_addCheckXC(dst, x, c, l)
//#define Int64_addCheckCX(dst, c, x, l) Int64_addCheckXC(dst, x, c, l)

#define Int8_addCheck Int8_addCheckXC
#define Int16_addCheck Int16_addCheckXC
#define Int32_addCheck Int32_addCheckXC
//#define Int64_addCheck Int64_addCheckXC

#define Int_negCheck(size, dst, n, l)		\
	do {					\
		if (n == Int##size##_min)	\
			goto l;			\
		dst = -n;			\
	} while (0)

#define Int8_negCheck(dst, n, l) Int_negCheck(8, dst, n, l)
#define Int16_negCheck(dst, n, l) Int_negCheck(16, dst, n, l)
#define Int32_negCheck(dst, n, l) Int_negCheck(32, dst, n, l)
//#define Int64_negCheck(dst, n, l) Int_negCheck(64, dst, n, l)

#define Int_subCheckCX(size, dst, c, x, l)		\
	do {						\
 		if (c >= 0) {				\
			if (x < c - Int##size##_max)	\
				goto l;			\
		} else if (x > c - Int##size##_min)	\
			goto l;				\
		dst = c - x;				\
	} while (0)
#define Int8_subCheckCX(dst, c, x, l) Int_subCheckCX(8, dst, c, x, l)
#define Int16_subCheckCX(dst, c, x, l) Int_subCheckCX(16, dst, c, x, l)
#define Int32_subCheckCX(dst, c, x, l) Int_subCheckCX(32, dst, c, x, l)
//#define Int64_subCheckCX(dst, c, x, l) Int_subCheckCX(64, dst, c, x, l)

#define Int_subCheckXC(size, dst, x, c, l)		\
	do {						\
		if (c <= 0) {				\
			if (x > Int##size##_max + c)	\
				goto l;			\
		} else if (x < Int##size##_min + c)	\
			goto l;				\
		dst = x - c;				\
 	} while (0)
#define Int8_subCheckXC(dst, c, x, l) Int_subCheckXC(8, dst, c, x, l)
#define Int16_subCheckXC(dst, c, x, l) Int_subCheckXC(16, dst, c, x, l)
#define Int32_subCheckXC(dst, c, x, l) Int_subCheckXC(32, dst, c, x, l)
//#define Int64_subCheckXC(dst, c, x, l) Int_subCheckXC(64, dst, c, x, l)

#define Int8_subCheck Int8_subCheckXC
#define Int16_subCheck Int16_subCheckXC
#define Int32_subCheck Int32_subCheckXC
//#define Int64_subCheck Int64_subCheckXC

#define Word_addCheckXC(size, dst, x, c, l)	\
	do {					\
		if (x > Word##size##_max - c)	\
			goto l;			\
		dst = x + c;			\
	} while (0)
#define Word8_addCheckXC(dst, x, c, l) Word_addCheckXC(8, dst, x, c, l)
#define Word16_addCheckXC(dst, x, c, l) Word_addCheckXC(16, dst, x, c, l)
#define Word32_addCheckXC(dst, x, c, l) Word_addCheckXC(32, dst, x, c, l)
//#define Word64_addCheckXC(dst, x, c, l) Word_addCheckXC(64, dst, x, c, l)
#define Word8_addCheckCX(dst, c, x, l) Word_addCheckXC(8, dst, x, c, l)
#define Word16_addCheckCX(dst, c, x, l) Word_addCheckXC(16, dst, x, c, l)
#define Word32_addCheckCX(dst, c, x, l) Word_addCheckXC(32, dst, x, c, l)
//#define Word64_addCheckCX(dst, c, x, l) Word_addCheckXC(64, dst, x, c, l)

#define Word8_addCheck Word8_addCheckXC
#define Word16_addCheck Word16_addCheckXC
#define Word32_addCheck Word32_addCheckXC
//#define Word64_addCheck Word64_addCheckXC

#define mulOverflow(kind, small, large)						\
	static inline kind##small kind##small##_##mulOverflow			\
			(kind##small x1, kind##small x2, Bool *overflow) {	\
		kind##large tmp;						\
		kind##small res;						\
										\
		tmp = (kind##large)x1 * x2;					\
		res = tmp;							\
		*overflow = (tmp != res);					\
		return res;							\
	}
mulOverflow(Int, 8, 16)
mulOverflow(Int, 16, 32)
mulOverflow(Int, 32, 64)
mulOverflow(Word, 8, 16)
mulOverflow(Word, 16, 32)
mulOverflow(Word, 32, 64)
#undef mulOverflow

#define check(dst, n1, n2, l, f);						\
	do {									\
		int overflow;							\
		dst = f (n1, n2, &overflow);					\
		if (DEBUG_CCODEGEN)						\
			fprintf (stderr, "%s:%d: " #f "(%d, %d) = %d\n",	\
					__FILE__, __LINE__, n1, n2, dst);	\
		if (overflow) {							\
			if (DEBUG_CCODEGEN)					\
				fprintf (stderr, "%s:%d: overflow\n",		\
						__FILE__, __LINE__);		\
			goto l;							\
		}								\
	} while (0)

#define Int8_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Int8_mulOverflow)
#define Int16_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Int16_mulOverflow)
#define Int32_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Int32_mulOverflow)
//#define Int64_mulCheck(dst, n1, n2, l)			\
//	fprintf (stderr, "FIXME: Int64_mulCheck\n");

#define Word8_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Word8_mulOverflow)
#define Word16_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Word16_mulOverflow)
#define Word32_mulCheck(dst, n1, n2, l)			\
	check (dst, n1, n2, l, Word32_mulOverflow)
//#define Word64_mulCheck(dst, n1, n2, l)			\
//	fprintf (stderr, "FIXME: Word64_mulCheck\n");

#endif /* INT_TEST */

#define intBinary(name, op, size)			\
	static inline Int##size Int##size##_##name 	\
			(Int##size i1, Int##size i2) {	\
		return i1 op i2;			\
	}
#define intAllBinary(name, op)			\
	intBinary(name,op,8)			\
	intBinary(name,op,16)			\
	intBinary(name,op,32)
//	intBinary(name,op,64)
intAllBinary (add, +)
intAllBinary (mul, *)
intAllBinary (sub, -)
#undef intBinary
#undef intAllBinary

#define intBinaryCompare(name, op, size) 		\
	static inline Bool Int##size##_##name 		\
			(Int##size i1, Int##size i2) {	\
		return i1 op i2;			\
	}
#define intAllBinaryCompare(name, op)		\
	intBinaryCompare(name,op,8)		\
	intBinaryCompare(name,op,16)		\
	intBinaryCompare(name,op,32)
//	intBinaryCompare(name,op,64)
intAllBinaryCompare (equal, ==)
intAllBinaryCompare (ge, >=)
intAllBinaryCompare (gt, >)
intAllBinaryCompare (le, <=)
intAllBinaryCompare (lt, <)
#undef intBinaryCompare
#undef intAllBinaryCompare

#define Int_neg(size)							\
	static inline Int##size Int##size##_##neg (Int##size i) {	\
		return -i;						\
	}
Int_neg(8)
Int_neg(16)
Int_neg(32)
//Int_neg(64)
#undef Int_neg

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

/* Used by polymorphic equality to implement equal on ground types
 * like char, int, word,  and on ref cells.
 * It is emitted by backend/machine.fun.
 */
#define MLton_eq(x, y) ((x) == (y))

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

Real64 atan2 (Real64 x, Real64 y);
#define Real64_Math_atan2 atan2
static inline Real32 Real32_Math_atan2 (Real32 x, Real32 y) {
	return (Real32)(Real64_Math_atan2 ((Real64)x, (Real64)y));
}

#define unaryReal(f,g)						\
	Real64 g (Real64 x);					\
	static inline Real64 Real64_Math_##f (Real64 x) {	\
		return g (x);					\
	}							\
	static inline Real32 Real32_Math_##f (Real32 x) {	\
		return (Real32)(Real64_Math_##f ((Real64)x));	\
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

#define binaryReal(name, op)						\
	static inline Real32 Real32_##name (Real32 x, Real32 y) {	\
		return x op y;						\
	}								\
	static inline Real64 Real64_##name (Real64 x, Real64 y) {	\
		return x op y;						\
	}
binaryReal(add, +)
binaryReal(div, /)
binaryReal(mul, *)
binaryReal(sub, -)

#undef binaryReal
#define binaryReal(name, op)					\
	static inline Bool Real32_##name (Real32 x, Real32 y) {	\
		return x op y;					\
	}							\
	static inline Bool Real64_##name (Real64 x, Real64 y) {	\
		return x op y;					\
	}
binaryReal(equal, ==)
binaryReal(ge, >=)
binaryReal(gt, >)
binaryReal(le, <=)
binaryReal(lt, <)

#define Real32_muladd(x, y, z) ((x) * (y) + (z))
#define Real32_mulsub(x, y, z) ((x) * (y) - (z))
#define Real64_muladd(x, y, z) ((x) * (y) + (z))
#define Real64_mulsub(x, y, z) ((x) * (y) - (z))
#define Real32_neg(x) (-(x))
#define Real64_neg(x) (-(x))
#define Real32_toInt(x) ((Int)(x))
#define Real64_toInt(x) ((Int)(x))

typedef volatile union {
	Word tab[2];
	Real64 d;
} Real64Or2Words;

static inline Real64 Real64_fetch (Real64 *dp) {
 	Real64Or2Words u;
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
 	Real64Or2Words u;
	Word32 *p;

	p = (Word32*)dp;
	u.d = d;
	p[0] = u.tab[0];
	p[1] = u.tab[1];
}

/* ------------------------------------------------- */
/*                        Word                       */
/* ------------------------------------------------- */

#define wordBinary(size, name, op)				\
	static inline Word##size Word##size##_##name 		\
			(Word##size w1, Word##size w2) {	\
		return w1 op w2;				\
	}
#define wordCmp(size, name, op)					\
	static inline Bool Word##size##_##name 			\
			(Word##size w1, Word##size w2) {	\
		return w1 op w2;				\
	}
#define wordShift(size, name, op)			\
	static inline Word##size Word##size##_##name 	\
			(Word##size w1, Word w2) {	\
		return w1 op w2;			\
	}
#define wordUnary(size, name, op)					\
	static inline Word##size Word##size##_##name (Word##size w) {	\
		return op w;						\
	}
#define wordOps(size)								\
	wordBinary (size, add, +)						\
	wordBinary (size, andb, &)						\
	wordBinary (size, div, /)						\
	wordBinary (size, mod, %)						\
	wordBinary (size, mul, *)						\
	wordBinary (size, orb, |)						\
	wordBinary (size, sub, -)						\
	wordBinary (size, xorb, ^)						\
	wordCmp (size, equal, ==)						\
	wordCmp (size, ge, >=)							\
	wordCmp (size, gt, >)							\
	wordCmp (size, le, <=)							\
	wordCmp (size, lt, <)							\
	wordShift (size, lshift, <<)						\
	wordShift (size, rshift, >>)						\
	wordUnary (size, neg, -)						\
	wordUnary (size, notb, ~)						\
	/* Word_arshift isn't ANSI C, because ANSI doesn't guarantee sign	\
         * extension.  We use it anyway cause it always seems to work.		\
	 */									\
	static inline Word##size Word##size##_arshift (Word##size w, Word s) {	\
		return (Int##size)w >> s;					\
	}									\
	static inline Word##size Word##size##_rol (Word##size w1, Word w2) {	\
		return (w1 >> (size - w2)) | (w1 << w2);			\
	}									\
	static inline Word##size Word##size##_ror (Word##size w1, Word w2) {	\
		return (w1 >> w2) | (w1 << (size - w2));			\
	}
wordOps(8)
wordOps(16)
wordOps(32)
//wordOps(64)
#undef wordBinary wordCmp wordShift wordUnary

#define coerce(f, t)				\
	static inline t f##_to##t (f x) {	\
		return (t)x;			\
	}
//coerce (Int64, Int64)
//coerce (Int64, Int32)
//coerce (Int64, Int16)
//coerce (Int64, Int8)
//coerce (Int32, Int64)
coerce (Int32, Int32)
coerce (Int32, Int16)
coerce (Int32, Int8)
//coerce (Int16, Int64)
coerce (Int16, Int32)
coerce (Int16, Int16)
coerce (Int16, Int8)
//coerce (Int8, Int64)
coerce (Int8, Int32)
coerce (Int8, Int16)
coerce (Int8, Int8)
//coerce (Int64, Real64)
//coerce (Int64, Real32)
coerce (Int32, Real64)
coerce (Int32, Real32)
coerce (Int16, Real64)
coerce (Int16, Real32)
coerce (Int8, Real64)
coerce (Int8, Real32)
//coerce (Int64, Word32)
//coerce (Int64, Word16)
//coerce (Int64, Word8)  
coerce (Int32, Word32)
coerce (Int32, Word16)
coerce (Int32, Word8)
coerce (Int16, Word32)
coerce (Int16, Word16)
coerce (Int16, Word8)
coerce (Int8, Word32)
coerce (Int8, Word16)
coerce (Int8, Word8)
//coerce (Real64, Int64)
coerce (Real64, Int32)
coerce (Real64, Int16)
coerce (Real64, Int8)
//coerce (Real32, Int64)
coerce (Real32, Int32)
coerce (Real32, Int16)
coerce (Real32, Int8)
coerce (Real64, Real64)
coerce (Real64, Real32)
coerce (Real32, Real64)
coerce (Real32, Real32)
//coerce (Word32, Int64)
coerce (Word32, Int32)
coerce (Word32, Int16)
coerce (Word32, Int8)
//coerce (Word16, Int64)
coerce (Word16, Int32)
coerce (Word16, Int16)
coerce (Word16, Int8)
//coerce (Word8, Int64)
coerce (Word8, Int32)
coerce (Word8, Int16)
coerce (Word8, Int8)
coerce (Word32, Word32)
coerce (Word32, Word16)
coerce (Word32, Word8)
coerce (Word16, Word32)
coerce (Word16, Word16)
coerce (Word16, Word8)
coerce (Word8, Word32)
coerce (Word8, Word16)
coerce (Word8, Word8)
#undef coerce

#define coerceX(size, t)					\
	static inline t Word##size##_to##t##X (Word##size x) {	\
		return (t)(Int##size)x;				\
	}
//coerceX (64, Int64)
//coerceX (64, Int32)
//coerceX (64, Int16)
//coerceX (64, Int8)
//coerceX (64, Word32)
//coerceX (64, Word16)
//coerceX (64, Word8)
//coerceX (64, Int64)
coerceX (32, Int32)
coerceX (32, Int16)
coerceX (32, Int8)
coerceX (32, Word32)
coerceX (32, Word16)
coerceX (32, Word8)
//coerceX (16, Int64)
coerceX (16, Int32)
coerceX (16, Int16)
coerceX (16, Int8)
coerceX (16, Word32)
coerceX (16, Word16)
coerceX (16, Word8)
//coerceX (8, Int64)
coerceX (8, Int32)
coerceX (8, Int16)
coerceX (8, Int8)
coerceX (8, Word32)
coerceX (8, Word16)
coerceX (8, Word8)
#undef coerceX

#endif /* #ifndef _C_CHUNK_H_ */
