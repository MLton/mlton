#ifndef _CCODEGEN_H_
#define _CCODEGEN_H_

#include "codegen.h"

/* Globals */
static pointer arrayAllocateRes;
static int nextFun;
static int sizeRes;
static pointer stackRes;

/* The CReturn's must be globals and cannot be per chunk because
 * they may be assigned in one chunk and read in another.  See, e.g.
 * Array_allocate.
 */
static char CReturnC;
static double CReturnD;
static int CReturnI;
static char *CReturnP;
static uint CReturnU;

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

#define IsInt(p) (0x3 & (int)(p))

#define BZ(x, l)						\
	do {							\
		if (DEBUG_CCODEGEN)				\
			fprintf (stderr, "%d  BZ(%d, %s)\n", 	\
					__LINE__, (x), #l); 	\
		if (0 == (x)) goto l;				\
	} while (0)

#define BNZ(x, l)						\
	do {							\
		if (DEBUG_CCODEGEN)				\
			fprintf (stderr, "%d  BNZ(%d, %s)\n",	\
					__LINE__, (x), #l);	\
		if (x) goto l;					\
	} while (0)

/* ------------------------------------------------- */
/*                       Chunk                       */
/* ------------------------------------------------- */

#define ChunkName(n) Chunk ## n

#define Chunkp(n) &(ChunkName(n))

struct cont {
	void *nextChunk;
};

#define DeclareChunk(n)				\
	static struct cont ChunkName(n)(void)

#define Chunk(n)				\
	DeclareChunk(n) {			\
		struct cont cont;		\
		int l_nextFun = nextFun;	\
		char *stackTop;			\
		pointer frontier;		\

#define ChunkSwitch(n)							\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%d  entering chunk %d\n",	\
					__LINE__, n);			\
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
				return(cont);				\
		} /* end switch (l_nextFun) */				\
		} /* end while (1) */					\
	} /* end chunk */

/* ------------------------------------------------- */
/*                       main                        */
/* ------------------------------------------------- */

#define Main(cs, mg, mfs, mlw, mmc, ps, mc, ml)				\
int main (int argc, char **argv) {					\
	struct cont cont;						\
	gcState.native = FALSE;						\
	Initialize(cs, mg, mfs, mlw, mmc, ps);				\
	if (gcState.isOriginal) {					\
		real_Init();						\
		PrepFarJump(mc, ml);					\
	} else {							\
		/* Return to the saved world */				\
		nextFun = *(int*)(gcState.stackTop - WORD_SIZE);	\
		cont.nextChunk = nextChunks[nextFun];			\
	}								\
	/* Trampoline */						\
	while (1) {							\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
	}								\
}

/* ------------------------------------------------- */
/*                      farJump                      */
/* ------------------------------------------------- */

#define PrepFarJump(n, l)				\
	do {						\
		cont.nextChunk = (void*)ChunkName(n);	\
		nextFun = l;				\
	} while (0)

#define FarJump(n, l)	 			\
	do {					\
		PrepFarJump(n, l); 		\
		goto leaveChunk;		\
	} while (0)

#define Reg(name, i) local ## name ## i
#define RC(n) Reg(c, n)
#define RD(n) Reg(d, n)
#define RI(n) Reg(i, n)
#define RP(n) Reg(p, n)
#define RU(n) Reg(u, n)

#define Declare(ty, name, i) ty Reg(name, i)
#define DC(n) Declare(uchar, c, n)
#define DD(n) Declare(double, d, n)
#define DI(n) Declare(int, i, n)
#define DP(n) Declare(pointer, p, n)
#define DU(n) Declare(uint, u, n)

#define Slot(ty, i) *(ty*)(stackTop + (i))
#define SC(i) Slot(uchar, i)
#define SD(i) Slot(double, i)
#define SI(i) Slot(int, i)
#define SP(i) Slot(pointer, i)
#define SU(i) Slot(uint, i)

#define Global(ty, i) (global ## ty [ i ])
#define GC(i) Global(uchar, i)
#define GD(i) Global(double, i)
#define GI(i) Global(int, i)
#define GP(i) Global(pointer, i)
#define GPNR(i) Global(pointerNonRoot, i)
#define GU(i) Global(uint, i)

#define Offset(ty, b, o) (*(ty*)((b) + (o)))
#define OC(b, i) Offset(uchar, b, i)
#define OD(b, i) Offset(double, b, i)
#define OI(b, i) Offset(int, b, i)
#define OP(b, i) Offset(pointer, b, i)
#define OU(b, i) Offset(uint, b, i)

#define Contents(t, x) (*(t*)(x))
#define CC(x) Contents(uchar, x)
#define CD(x) Contents(double, x)
#define CI(x) Contents(int, x)
#define CP(x) Contents(pointer, x)
#define CU(x) Contents(uint, x)

/* ------------------------------------------------- */
/*                       Stack                       */
/* ------------------------------------------------- */

#define ExnStack gcState.currentThread->exnStack
#define StackBottom gcState.stackBottom

#define Push(bytes)					\
	do {						\
		stackTop += (bytes);			\
		assert(StackBottom <= stackTop);	\
	} while (0)

#define Return()								\
	do {									\
		l_nextFun = *(word*)(stackTop - WORD_SIZE);			\
		if (DEBUG_CCODEGEN)						\
			fprintf (stderr, "%d  Return()  l_nextFun = %d\n",	\
					__LINE__, l_nextFun);			\
		goto top;							\
	} while (0)

#define Raise()								\
	do {								\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%d  Raise\n", __LINE__);	\
		stackTop = StackBottom + ExnStack;			\
		Return();						\
	} while (0)

/* ------------------------------------------------- */
/*                      Runtime                      */
/* ------------------------------------------------- */

#define ProfileLabel(l)				\
	__asm__ __volatile__ (#l ## ":" : : )

#define CheckPointer(p)						\
	do {							\
		assert (not GC_isPointer (p)			\
			or (gcState.heap.start <= p 		\
				and p < gcState.heap.start 	\
					+ gcState.oldGenSize)	\
			or (gcState.nursery <= p 		\
				and p < frontier));		\
	} while (0)

#define FlushFrontier()				\
	do {					\
		gcState.frontier = frontier;	\
	} while (0)

#define FlushStackTop()				\
	do {					\
		gcState.stackTop = stackTop;	\
	} while (0)

#define CacheFrontier()				\
	do {					\
		frontier = gcState.frontier;	\
	} while (0)

#define CacheStackTop()				\
	do {					\
		stackTop = gcState.stackTop;	\
	} while (0)

#define SmallIntInf(n) ((pointer)(n))
#define IntAsPointer(n) ((pointer)(n))
#define PointerToInt(p) ((int)(p))

#define Object(x, h)							\
	do {								\
		*(word*)frontier = (h);					\
		x = frontier + GC_NORMAL_HEADER_SIZE;			\
		if (DEBUG_CCODEGEN)					\
			fprintf (stderr, "%d  0x%x = Object(%d)\n",	\
				 __LINE__, x, h);			\
		assert (frontier <= gcState.limitPlusSlop);		\
	} while (0)

#define Assign(ty, o, v)						\
	do {								\
		*(ty*)(frontier + GC_NORMAL_HEADER_SIZE + (o)) = (v);	\
	} while (0)

#define AC(o, x) Assign(uchar, o, x)
#define AD(o, x) Assign(double, o, x)
#define AI(o, x) Assign(int, o, x)
#define AP(o, x) Assign(pointer, o, x)
#define AU(o, x) Assign(uint, o, x)

#define EndObject(bytes)					\
	do {							\
		frontier += (bytes);				\
	} while (0)

/* ------------------------------------------------- */
/*                      Arrays                       */
/* ------------------------------------------------- */

#define Array_length GC_arrayNumElements

#define ArrayOffset(ty, b, i) (*(ty*)((b) + ((i) * sizeof(ty))))

#define XC(b, i) ArrayOffset(uchar, b, i)
#define XD(b, i) ArrayOffset(double, b, i)
#define XI(b, i) ArrayOffset(int, b, i)
#define XP(b, i) ArrayOffset(pointer, b, i)
#define XU(b, i) ArrayOffset(uint, b, i)

/* ------------------------------------------------- */
/*                       Char                        */
/* ------------------------------------------------- */

#define Char_lt(c1, c2) ((c1) < (c2))
#define Char_le(c1, c2) ((c1) <= (c2))
#define Char_gt(c1, c2) ((c1) > (c2))
#define Char_ge(c1, c2) ((c1) >= (c2))
#define Char_chr(c) ((uchar)(c))
#define Char_ord(c) ((int)(c))

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

static inline Int Int_addOverflow(Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs + rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
static inline Int Int_mulOverflow(Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs * rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
static inline Int Int_subOverflow(Int lhs, Int rhs, Bool *overflow) {
	long long	tmp;

	tmp = (long long)lhs - rhs;
	*overflow = (tmp != (int)tmp);
	return tmp;
}

#if (defined (INT_TEST) || defined (INT_LONG))
#define check(dst, n1, n2, l, f);						\
	do {									\
		int overflow;							\
		dst = f(n1, n2, &overflow);					\
		if (DEBUG_CCODEGEN)						\
			fprintf(stderr, #f "(%d, %d) = %d\n", n1, n2, dst);	\
		if (overflow) {							\
			if (DEBUG_CCODEGEN)					\
				fprintf(stderr, "overflow\n");			\
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
#define Int_geu(x, y) ((uint)(x) >= (uint)(y))
#define Int_gtu(x, y) ((uint)(x) > (uint)(y))
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

#include <math.h>
#define Real_Math_acos acos
#define Real_Math_asin asin
#define Real_Math_atan atan
#define Real_Math_atan2 atan2
#define Real_Math_cos cos
#define Real_Math_cosh cosh
#define Real_Math_exp exp
#define Real_Math_ln log
#define Real_Math_log10 log10
#define Real_Math_pow pow
#define Real_Math_sin sin
#define Real_Math_sinh sinh
#define Real_Math_sqrt sqrt
#define Real_Math_tan tan
#define Real_Math_tanh tanh

#define Real_abs fabs
#define Real_add(x, y) ((x) + (y))
#define Real_copysign copysign
#define Real_div(x, y) ((x) / (y))
#define Real_equal(x1, x2) ((x1) == (x2))
#define Real_frexp frexp
#define Real_fromInt(n) ((double)(n))
#define Real_ge(x1, x2) ((x1) >= (x2))
#define Real_gt(x1, x2) ((x1) > (x2))
#define Real_ldexp ldexp
#define Real_le(x1, x2) ((x1) <= (x2))
#define Real_lt(x1, x2) ((x1) < (x2))
#define Real_modf modf
#define Real_mul(x, y) ((x) * (y))
#define Real_muladd(x, y, z) ((x) * (y) + (z))
#define Real_mulsub(x, y, z) ((x) * (y) - (z))
#define Real_neg(x) (-(x))
#define Real_sub(x, y) ((x) - (y))
#define Real_toInt(x) ((int)(x))

/* ------------------------------------------------- */
/*                      Vector                       */
/* ------------------------------------------------- */

#define Vector_length GC_arrayNumElements

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
#define Word8_fromInt(x) ((uchar)(x))
#define Word8_fromLargeWord(w) ((uchar)(w))
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
#define Word8_ror(x, y) ((x)>>(y) | ((x)<<(8-(y))))
#define Word8_rol(x, y) ((x)>>(8-(y)) | ((x)<<(y)))
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

#endif /* #ifndef _CCODEGEN_H_ */
