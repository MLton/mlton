#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

#include "my-lib.h"
#include "c-common.h"

#define WORD_SIZE 4

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

typedef unsigned char Char;
typedef double Double;
typedef int Int;
typedef char *Pointer;
typedef unsigned long Word32;
typedef Word32 Word;
typedef unsigned long long Word64;

#define Bool Int

extern Char CReturnC;
extern Double CReturnD;
extern Int CReturnI;
extern Char *CReturnP;
extern Word CReturnU;
extern struct cont (*nextChunks []) ();
extern Int nextFun;
extern Int returnToC;
extern struct GC_state gcState;
extern Char globaluchar[];
extern Double globaldouble[];
extern Int globalint[];
extern Pointer globalpointer[];
extern Word globaluint[];
extern Pointer globalpointerNonRoot[];

#define GCState ((Pointer)&gcState)
#define ExnStack *(Word*)(GCState + ExnStackOffset)
#define FrontierMem *(Word*)(GCState + FrontierOffset)
#define Frontier frontier
#define StackBottom *(Word*)(GCState + StackBottomOffset)
#define StackTopMem *(Word*)(GCState + StackTopOffset)
#define StackTop stackTop

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

#define Thread_returnToC()							\
	do {									\
		if (DEBUG_CCODEGEN)						\
			fprintf (stderr, "%s:%d: Thread_returnToC()\n",	\
					__FILE__, __LINE__);			\
		returnToC = TRUE;						\
		return cont;							\
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
/*                      Globals                      */
/* ------------------------------------------------- */

#define Global(ty, i) (global ## ty [ i ])
#define GC(i) Global(uchar, i)
#define GD(i) Global(double, i)
#define GI(i) Global(int, i)
#define GP(i) Global(pointer, i)
#define GPNR(i) Global(pointerNonRoot, i)
#define GU(i) Global(uint, i)

/* ------------------------------------------------- */
/*                     Registers                     */
/* ------------------------------------------------- */

#define Declare(ty, name, i) ty Reg(name, i)
#define DC(n) Declare(Char, c, n)
#define DD(n) Declare(Double, d, n)
#define DI(n) Declare(Int, i, n)
#define DP(n) Declare(Pointer, p, n)
#define DU(n) Declare(Word, u, n)

#define Reg(name, i) local ## name ## i
#define RC(n) Reg(c, n)
#define RD(n) Reg(d, n)
#define RI(n) Reg(i, n)
#define RP(n) Reg(p, n)
#define RU(n) Reg(u, n)

/* ------------------------------------------------- */
/*                      Memory                       */
/* ------------------------------------------------- */

#define Offset(ty, b, o) (*(ty*)((b) + (o)))
#define OC(b, i) Offset(Char, b, i)
#define OD(b, i) Offset(Double, b, i)
#define OI(b, i) Offset(Int, b, i)
#define OP(b, i) Offset(Pointer, b, i)
#define OU(b, i) Offset(Word, b, i)

#define Contents(t, x) (*(t*)(x))
#define CC(x) Contents(Char, x)
#define CD(x) Contents(Double, x)
#define CI(x) Contents(Int, x)
#define CP(x) Contents(Pointer, x)
#define CU(x) Contents(Word, x)

/* ------------------------------------------------- */
/*                       Stack                       */
/* ------------------------------------------------- */

#define Slot(ty, i) *(ty*)(StackTop + (i))
#define SC(i) Slot(Char, i)
#define SD(i) Slot(Double, i)
#define SI(i) Slot(Int, i)
#define SP(i) Slot(Pointer, i)
#define SU(i) Slot(Word, i)

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
		StackTop = StackBottom + ExnStack;	\
		Return();							\
	} while (0)								\

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
/*                      Arrays                       */
/* ------------------------------------------------- */

#define ArrayOffset(ty, b, i) (*(ty*)((b) + ((i) * sizeof(ty))))

#define XC(b, i) ArrayOffset (Char, b, i)
#define XD(b, i) ArrayOffset (Double, b, i)
#define XI(b, i) ArrayOffset (Int, b, i)
#define XP(b, i) ArrayOffset (Pointer, b, i)
#define XU(b, i) ArrayOffset (Word, b, i)

/* ------------------------------------------------- */
/*                       Char                        */
/* ------------------------------------------------- */

#define Char_lt(c1, c2) ((c1) < (c2))
#define Char_le(c1, c2) ((c1) <= (c2))
#define Char_gt(c1, c2) ((c1) > (c2))
#define Char_ge(c1, c2) ((c1) >= (c2))
#define Char_chr(c) ((Char)(c))
#define Char_ord(c) ((Int)(c))

/* ------------------------------------------------- */
/*                     Cpointer                      */
/* ------------------------------------------------- */

#define Cpointer_isNull(x) (NULL == (void*)(x))

/* ------------------------------------------------- */
/*                        Int                        */
/* ------------------------------------------------- */

/* The old -DFAST_INT has been renamed to -DINT_JO. */
#if (defined (FAST_INT))
#define INT_JO
#endif

/* The default is to use INT_TEST. */
#if (! defined (INT_NO_CHECK) && ! defined (INT_JO) && ! defined (INT_TEST) && ! defined (INT_LONG))
#define INT_TEST
#endif

enum {
	MAXINT = 0x7FFFFFFF,
	MININT = (int)0x80000000,
	MAXWORD = 0xFFFFFFFF,
};

#if (defined (INT_NO_CHECK))
#define Int_addCheck(dst, n1, n2, l) dst = n1 + n2
#define Int_mulCheck(dst, n1, n2, l) dst = n1 * n2
#define Int_negCheck(dst, n, l) dst = -n
#define Int_subCheck(dst, n1, n2, l) dst = n1 - n2
#define Word32_addCheck(dst, n1, n2, l) dst = n1 + n2
#define Word32_mulCheck(dst, n1, n2, l) dst = n1 * n2
#endif

#if (defined (INT_TEST))
#define Int_addCheckXC(dst, x, c, l) 		\
	do {					\
		if (c >= 0) {			\
			if (x > MAXINT - c)	\
				goto l;		\
		} else if (x < MININT - c)	\
				goto l;		\
		dst = x + c;			\
	} while (0)
#define Int_addCheckCX(dst, c, x, l) Int_addCheckXC(dst, x, c, l)
#define Int_subCheckCX(dst, c, x, l)		\
	do {					\
 		if (c >= 0) {			\
			if (x < c - MAXINT)	\
				goto l;		\
		} else if (x > c - MININT)	\
			goto l;			\
		dst = c - x;			\
	} while (0)
#define Int_subCheckXC(dst, x, c, l)		\
	do {					\
		if (c <= 0) {			\
			if (x > MAXINT + c)	\
				goto l;		\
		} else if (x < MININT + c)	\
			goto l;			\
		dst = x - c;			\
 	} while (0)
#define Word32_addCheckXC(dst, x, c, l)		\
	do {					\
		if (x > MAXWORD - c)		\
			goto l;			\
		dst = x + c;			\
	} while (0)
#define Word32_addCheckCX(dst, c, x, l) Word32_addCheckXC(dst, x, c, l)

#define Int_addCheck Int_addCheckXC
#define Int_subCheck Int_subCheckXC
#define Word32_addCheck Word32_addCheckXC

#endif

static inline Int Int_addOverflow (Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs + rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
static inline Int Int_mulOverflow (Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs * rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
static inline Int Int_subOverflow (Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs - rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
static inline Word32 Word32_addOverflow (Word32 lhs, Word32 rhs, Bool *overflow) {
	Word64 tmp;

	tmp = (Word64)lhs + rhs;
	*overflow = (tmp != (Word32)tmp);
	return tmp;
}
static inline Word32 Word32_mulOverflow (Word32 lhs, Word32 rhs, Bool *overflow) {
	Word64 tmp;

	tmp = (Word64)lhs * rhs;
	*overflow = (tmp != (Word32)tmp);
	return tmp;
}

#if (defined (INT_TEST) || defined (INT_LONG))
#define check(dst, n1, n2, l, f);						\
	do {									\
		int overflow;							\
		dst = f(n1, n2, &overflow);					\
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
#define Int_mulCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_mulOverflow)
#define Int_negCheck(dst, n, l)			\
	do {					\
		if (n == MININT)		\
			goto l;			\
		dst = -n;			\
	} while (0)
#define Word32_mulCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Word32_mulOverflow)
#endif

#if (defined (INT_LONG))
#define Int_addCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_addOverflow)
#define Int_subCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_subOverflow)
#define Word32_addCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Word32_addOverflow)
#endif

#if (defined (INT_JO))

static void MLton_overflow () {
	die("Internal overflow detected. Halt.");
}

static inline Int Int_addCheckFast (Int n1, Int n2) {
 	__asm__ __volatile__ ("addl %1, %0\n\tjo MLton_overflow"
			      : "+r" (n1) : "g" (n2) : "cc");

	return n1;
}

static inline Int Int_mulCheckFast (Int n1, Int n2) {
 	__asm__ __volatile__ ("imull %1, %0\n\tjo MLton_overflow"
			      : "+r" (n1) : "g" (n2) : "cc");

	return n1;
}

static inline Int Int_negCheckFast (Int n) {
	__asm__ __volatile__ ("negl %1\n\tjo MLton_overflow"
				: "+r" (n) : : "cc" );
	return n;
}

static inline Int Int_subCheckFast (Int n1, Int n2) {
 	__asm__ __volatile__ ("subl %1, %0\n\tjo MLton_overflow"
			      : "+r" (n1) : "g" (n2) : "cc" );

	return n1;
}

static inline Word Word32_addCheckFast (Word n1, Word n2) {
 	__asm__ __volatile__ ("addl %1, %0\n\tjc MLton_overflow"
			      : "+r" (n1) : "g" (n2) : "cc");

	return n1;
}

static inline Word Word32_mulCheckFast (Word n1, Word n2) {
 	__asm__ __volatile__ ("imull %1, %0\n\tjc MLton_overflow"
			      : "+r" (n1) : "g" (n2) : "cc");

	return n1;
}

#define check(dst,n1,n2,l,f) dst = f(n1, n2)

#define Int_addCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_addCheckFast)
#define Int_mulCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_mulCheckFast)
#define Int_negCheck(dst, n, l) 			\
	dst = Int_negCheckFast(n)
#define Int_subCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Int_subCheckFast)
#define Word32_addCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Word32_addCheckFast)
#define Word32_mulCheck(dst, n1, n2, l)			\
	check(dst, n1, n2, l, Word32_mulCheckFast)

#endif

#if (defined (INT_NO_CHECK) || defined (INT_JO) || defined (INT_LONG))
#define Int_addCheckCX Int_addCheck
#define Int_addCheckXC Int_addCheck
#define Int_subCheckCX Int_subCheck
#define Int_subCheckXC Int_subCheck
#define Word32_addCheckCX Word32_addCheck
#define Word32_addCheckXC Word32_addCheck
#endif

#define Int_add(n1, n2) ((n1) + (n2))
#define Int_mul(n1, n2) ((n1) * (n2))
#define Int_sub(n1, n2) ((n1) - (n2))
#define Int_lt(n1, n2) ((n1) < (n2))
#define Int_le(n1, n2) ((n1) <= (n2))
#define Int_gt(n1, n2) ((n1) > (n2))
#define Int_ge(n1, n2) ((n1) >= (n2))
#define Int_geu(x, y) ((Word)(x) >= (Word)(y))
#define Int_gtu(x, y) ((Word)(x) > (Word)(y))
#define Int_neg(n) (-(n))

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

Double acos (Double x);
#define Real_Math_acos acos
Double asin (Double x);
#define Real_Math_asin asin
Double atan (Double x);
#define Real_Math_atan atan
Double atan2 (Double x, Double y);
#define Real_Math_atan2 atan2
Double cos (Double x);
#define Real_Math_cos cos
Double cosh (Double x);
#define Real_Math_cosh cosh
Double exp (Double x);
#define Real_Math_exp exp
Double log (Double x);
#define Real_Math_ln log
Double log10 (Double x);
#define Real_Math_log10 log10
Double pow (Double x, Double y);
#define Real_Math_pow pow
Double sin (Double x);
#define Real_Math_sin sin
Double sinh (Double x);
#define Real_Math_sinh sinh
Double sqrt (Double x);
#define Real_Math_sqrt sqrt
Double tan (Double x);
#define Real_Math_tan tan
Double tanh (Double x);
#define Real_Math_tanh tanh

#define Real_abs fabs
#define Real_add(x, y) ((x) + (y))
#define Real_copysign copysign
#define Real_div(x, y) ((x) / (y))
#define Real_equal(x1, x2) ((x1) == (x2))
#define Real_fromInt(n) ((Double)(n))
#define Real_ge(x1, x2) ((x1) >= (x2))
#define Real_gt(x1, x2) ((x1) > (x2))
Double ldexp (Double x, Int i);
#define Real_ldexp ldexp
#define Real_le(x1, x2) ((x1) <= (x2))
#define Real_lt(x1, x2) ((x1) < (x2))
#define Real_mul(x, y) ((x) * (y))
#define Real_muladd(x, y, z) ((x) * (y) + (z))
#define Real_mulsub(x, y, z) ((x) * (y) - (z))
#define Real_neg(x) (-(x))
Int Real_qequal (Double x1, Double x2);
Double Real_round (Double x);
#define Real_sub(x, y) ((x) - (y))
#define Real_toInt(x) ((int)(x))

typedef volatile union {
	Word tab[2];
	Double d;
} DoubleOr2Words;

static inline double Real_fetch (double *dp) {
 	DoubleOr2Words u;
	Word32 *p;

	p = (Word32*)dp;
	u.tab[0] = p[0];
	u.tab[1] = p[1];
 	return u.d;
}

static inline void Real_move (double *dst, double *src) {
	Word32 *pd;
	Word32 *ps;
	Word32 t;

	pd = (Word32*)dst;
	ps = (Word32*)src;
	t = ps[1];
	pd[0] = ps[0];
	pd[1] = t;		
}

static inline void Real_store (double *dp, double d) {
 	DoubleOr2Words u;
	Word32 *p;

	p = (Word32*)dp;
	u.d = d;
	p[0] = u.tab[0];
	p[1] = u.tab[1];
}

/* ------------------------------------------------- */
/*                       Word8                       */
/* ------------------------------------------------- */

#define Word8_add(w1, w2) ((w1) + (w2))
#define Word8_andb(w1, w2) ((w1) & (w2))
/* The macro for Word8_arshift isn't ANSI C, because ANSI doesn't guarantee 
 * sign extension.  We use it anyway cause it always seems to work.
 */
#define Word8_arshift(w, s) ((signed char)(w) >> (s))
#define Word8_div(w1, w2) ((w1) / (w2))
#define Word8_fromInt(x) ((Char)(x))
#define Word8_fromLargeWord(w) ((Char)(w))
#define Word8_ge(w1, w2) ((w1) >= (w2))
#define Word8_gt(w1, w2) ((w1) > (w2))
#define Word8_le(w1, w2) ((w1) <= (w2))
#define Word8_lshift(w, s)  ((w) << (s))
#define Word8_lt(w1, w2) ((w1) < (w2))
#define Word8_mod(w1, w2) ((w1) % (w2))
#define Word8_mul(w1, w2) ((w1) * (w2))
#define Word8_neg(w) (-(w))
#define Word8_notb(w) (~(w))
#define Word8_orb(w1, w2) ((w1) | (w2))
#define Word8_rol(x, y) ((x)>>(8-(y)) | ((x)<<(y)))
#define Word8_ror(x, y) ((x)>>(y) | ((x)<<(8-(y))))
#define Word8_rshift(w, s) ((w) >> (s))
#define Word8_sub(w1, w2) ((w1) - (w2))
#define Word8_toInt(w) ((int)(w))
#define Word8_toIntX(x) ((int)(signed char)(x))
#define Word8_toLargeWord(w) ((uint)(w))
#define Word8_toLargeWordX(x) ((uint)(signed char)(x))
#define Word8_xorb(w1, w2) ((w1) ^ (w2))

/* ------------------------------------------------- */
/*                    Word8Array                     */
/* ------------------------------------------------- */

#define Word8Array_subWord(a, i) (((Word*)(a))[i])
#define Word8Array_updateWord(a, i, w) ((Word*)(a))[i] = (w)

/* ------------------------------------------------- */
/*                    Word8Vector                    */
/* ------------------------------------------------- */

#define Word8Vector_subWord(a, i) (((Word*)(a))[i])

/* ------------------------------------------------- */
/*                      Word32                       */
/* ------------------------------------------------- */

#define Word32_add(w1,w2) ((w1) + (w2))
#define Word32_andb(w1,w2) ((w1) & (w2))
/* The macro for Word32_arshift isn't ANSI C, because ANSI doesn't guarantee 
 * sign extension.  We use it anyway cause it always seems to work.
 * We do it because using a procedure call slows down IntInf by a factor of 2.
 */
#define Word32_arshift(w, s) ((int)(w) >> (s))
#define Word32_div(w1, w2) ((w1) / (w2))
#define Word32_ge(w1, w2) ((w1) >= (w2))
#define Word32_gt(w1, w2) ((w1) > (w2))
#define Word32_le(w1, w2) ((w1) <= (w2))
#define Word32_lshift(w, s) ((w) << (s))
#define Word32_lt(w1, w2) ((w1) < (w2))
#define Word32_mod(w1, w2) ((w1) % (w2))
#define Word32_mul(w1, w2) ((w1) * (w2))
#define Word32_neg(w) (-(w))
#define Word32_notb(w) (~(w))
#define Word32_orb(w1, w2) ((w1) | (w2))
#define Word32_ror(x, y) ((x)>>(y) | ((x)<<(32-(y))))
#define Word32_rol(x, y) ((x)>>(32-(y)) | ((x)<<(y)))
#define Word32_rshift(w, s) ((w) >> (s))
#define Word32_sub(w1, w2) ((w1) - (w2))
#define Word32_xorb(w1, w2) ((w1) ^ (w2))

#endif /* #ifndef _C_CHUNK_H_ */
