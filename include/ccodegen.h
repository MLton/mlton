#ifndef _CCODEGEN_H_
#define _CCODEGEN_H_

#ifndef GC_FIRST_CHECK
#define GC_FIRST_CHECK FALSE
#endif
#ifndef GC_EVERY_CHECK
#define GC_EVERY_CHECK FALSE
#endif

#if	GC_FIRST_CHECK
#define declareFirst	static bool	gc_first = TRUE
#define	clearFirst	gc_first = FALSE
#else
#define	declareFirst
#define	clearFirst
#define	gc_first	FALSE
#endif

#define Globals(c, d, i, p, u, nr)						\
	/* gcState can't be static because stuff in mlton-lib.c refers to it */	\
	struct GC_state gcState;						\
	static int sizeRes;							\
	static pointer serializeRes;						\
	static pointer deserializeRes;						\
	static pointer stackRes;						\
	static struct intInfRes_t *intInfRes;					\
	static void (*nextChunk)();						\
	static int nextFun;							\
	static char globaluchar[c];						\
	static double globaldouble[d];						\
	static int globalint[i];						\
	static pointer globalpointer[p];					\
	static uint globaluint[u];						\
	static pointer globalpointerNonRoot[nr];				\
	static void saveGlobals(FILE *file) {					\
		swrite(globaluchar, sizeof(char), c, file);			\
		swrite(globaldouble, sizeof(double), d, file);			\
		swrite(globalint, sizeof(int), i, file);			\
		swrite(globalpointer, sizeof(pointer), p, file);		\
		swrite(globaluint, sizeof(uint), u, file);			\
	}									\
	static void loadGlobals(FILE *file) {					\
		sread(globaluchar, sizeof(char), c, file);			\
		sread(globaldouble, sizeof(double), d, file);			\
		sread(globalint, sizeof(int), i, file);				\
		sread(globalpointer, sizeof(pointer), p, file);			\
		sread(globaluint, sizeof(uint), u, file);			\
	}

#ifdef GLOBAL_REGS
#define Locals(c, d, i, p, u)						\
	static char localc[c];						\
	static double locald[d];				       	\
	static int locali[i];						\
	static pointer localp[p];					\
	static uint localu[u]
#else
#define Locals(c, d, i, p, u) 
#endif					       	

#define BeginIntInfs static struct intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs { 0, NULL }};

#define BeginStrings static struct GC_stringInit stringInits[] = {
#define String(g, s, l) { g, s, l },
#define EndStrings { 0, NULL, 0 }};

#define BeginFloats static void float_Init() {
#define Float(c, f) globaldouble[c] = f;
#define EndFloats }

#define IsInt(p) (0x3 & (int)(p))

#define BZ(x, l)				\
	do {					\
		if (x == 0) goto l;		\
	} while (0)

/* ------------------------------------------------- */
/*                       Chunk                       */
/* ------------------------------------------------- */

#define ChunkName(n) Chunk ## n

#define Chunkp(n) &(ChunkName(n))

#define DeclareChunk(n)				\
	static void ChunkName(n)()

#define Chunk(n)				\
	static void ChunkName(n)() {		\
		char *stackTop;			\
		pointer frontier

#define ChunkSwitch				\
		CacheGC();			\
		top:				\
		switch (nextFun) {

#define EndChunk						\
		default:					\
			/* interchunk return */			\
			nextChunk = nextChunks[nextFun];	\
			leaveChunk:				\
				FlushGC();			\
				return;				\
		} /* end switch (nextFun) */			\
	} /* end chunk */

/* ------------------------------------------------- */
/*                       main                        */
/* ------------------------------------------------- */

#define Main(ufh, fs, bl, mfs, mfi, mg, mc, ml)				\
int main(int argc, char **argv) {					\
	gcState.useFixedHeap = ufh;					\
	gcState.fromSize = fs;						\
	gcState.bytesLive = bl;						\
	gcState.maxFrameSize = mfs;					\
	gcState.magic = mg;						\
	gcState.numGlobals = cardof(globalpointer);			\
	gcState.globals = globalpointer;				\
	gcState.maxFrameIndex = mfi;					\
	gcState.frameLayouts = frameLayouts;				\
	gcState.native = FALSE;       					\
	if (MLton_init(argc, argv, &loadGlobals)) {			\
 		/* The (> 1) check is so that the C compiler can	\
		 * eliminate the call if there are no IntInfs and we	\
		 * then won't have to link in with the IntInf stuff.	\
		 */							\
		if (cardof(intInfInits) > 1)				\
			IntInf_init(&gcState, intInfInits);		\
		GC_createStrings(&gcState, stringInits);		\
		float_Init();						\
		PrepFarJump(mc, ml);					\
	} else {							\
		/* Return to the saved world */				\
		nextFun = *(int*)gcState.stackTop;			\
		nextChunk = nextChunks[nextFun];			\
	}								\
	/* Trampoline */						\
	while (1) {							\
		(*nextChunk)();						\
	}								\
}

/* ------------------------------------------------- */
/*                      farJump                      */
/* ------------------------------------------------- */

#define PrepFarJump(n, l)			\
	do {					\
		nextChunk = ChunkName(n);	\
		nextFun = l ## _index;		\
	} while (0)

#define FarJump(n, l)	 			\
	do {					\
		PrepFarJump(n, l); 		\
		goto leaveChunk;		\
	} while (0)


#ifdef GLOBAL_REGS
#define Reg(name, i) (local ## name [ i ])
#define Declare(ty, name, i) 
#else
#define Reg(name, i) local ## name ## i
#define Declare(ty, name, i) ty Reg(name, i)
#endif 

#define RC(n) Reg(c, n)
#define RD(n) Reg(d, n)
#define RI(n) Reg(i, n)
#define RP(n) Reg(p, n)
#define RU(n) Reg(u, n)

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

#define Push(bytes)							\
	do {								\
		stackTop += (bytes);					\
		assert(gcState.stackBottom <= stackTop + WORD_SIZE);	\
	} while (0)

#define Return()				\
	do {					\
		nextFun = *(int*)stackTop;	\
		goto top;			\
	} while (0)

#define ExnStack gcState.currentThread->exnStack
#define StackBottom gcState.stackBottom

#define Raise()							\
	do {							\
		stackTop = StackBottom + ExnStack;		\
		nextFun = *(int*)stackTop;			\
		goto top;					\
	} while (0)

#define SetExnStackLocal(offset)				\
	do {							\
		ExnStack = stackTop + (offset) - StackBottom;	\
	} while (0)

#define SetSlotExnStack(offset)						\
	do {								\
		*(uint*)(stackTop + (offset) + WORD_SIZE) = ExnStack;	\
	} while (0)

#define SetExnStackSlot(offset) 					\
	do {								\
		ExnStack = *(uint*)(stackTop + (offset) + WORD_SIZE);	\
	} while (0)

/* ------------------------------------------------- */
/*                      Runtime                      */
/* ------------------------------------------------- */

#define CacheGC()				\
	do {					\
		stackTop = gcState.stackTop;	\
		frontier = gcState.frontier;	\
	} while (0)

#define FlushGC()				\
	do {					\
		gcState.stackTop = stackTop;   	\
		gcState.frontier = frontier;	\
	} while (0)

/* Be very careful when using this macro, since the "call" is moved to after
 * the stackTop change.  Thus, the call should not refer to stuff on the stack.
 */
#define InvokeRuntime(call, frameSize, ret)		\
	do {						\
		stackTop += (frameSize);		\
		*(uint*)stackTop = (ret ## _index);	\
		FlushGC();				\
		call;					\
		CacheGC();				\
		Return();				\
		ret:					\
		stackTop -= (frameSize);		\
	} while (0)

#define GC_collect(frameSize, ret)						\
	do {									\
		InvokeRuntime(GC_gc(&gcState, 0, TRUE, __FILE__, __LINE__),	\
				frameSize, ret);				\
	} while (0)

#define SmallIntInf(n) ((pointer)(n))
#define IntAsPointer(n) ((pointer)(n))
#define PointerToInt(p) ((int)(p))

#define Object(x, np, p)					\
	do {							\
		*(word*)frontier = GC_objectHeader(np, p);	\
		x = frontier + GC_OBJECT_HEADER_SIZE;		\
	} while (0)

#define Assign(ty, o, v)						\
	do {								\
		*(ty*)(frontier + GC_OBJECT_HEADER_SIZE + (o)) = (v);	\
	} while (0)

#define AC(o, x) Assign(uchar, o, x)
#define AD(o, x) Assign(double, o, x)
#define AI(o, x) Assign(int, o, x)
#define AP(o, x) Assign(pointer, o, x)
#define AU(o, x) Assign(uint, o, x)

#define EndObject(bytes)					\
	do {							\
		frontier += GC_OBJECT_HEADER_SIZE + (bytes);	\
	} while (0)

#define LimitCheck(frameSize, ret, b, other)				\
	do {								\
		declareFirst;						\
									\
		if (GC_EVERY_CHECK					\
		or (GC_FIRST_CHECK and gc_first)			\
		or frontier + (b) > gcState.limit			\
		or (other)) {						\
			uint bytes = b;					\
									\
			InvokeRuntime					\
			(GC_gc(&gcState, bytes,				\
				GC_EVERY_CHECK or			\
				(GC_FIRST_CHECK and gc_first),		\
				__FILE__, __LINE__),			\
			frameSize, ret);				\
			clearFirst;					\
		}							\
		assert(gcState.stackBottom <= stackTop + WORD_SIZE);	\
	} while (0)

#define StackOverflowCheck (stackTop >= gcState.stackLimit)

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

/* The extra POINTER_SIZE added to frontier is so space is allocated for the
 * forwarding pointer in zero length arrays.
 */
#define ArrayNoPointers(dst, numElts, bytesPerElt)				\
	do {									\
		assert(numElts >= 0);						\
		assert(bytesPerElt >= 0);					\
		*(word*)frontier = (numElts);					\
		*(word*)(frontier + WORD_SIZE) =				\
			GC_arrayHeader((bytesPerElt), 0);			\
		(dst) = frontier + 2 * WORD_SIZE;				\
		frontier = (dst) + ((0 == numElts || 0 == bytesPerElt)		\
				? POINTER_SIZE					\
				: wordAlign((numElts) * (bytesPerElt)));	\
	} while (0)

#define ArrayPointers(dst, numElts, numPointers)				\
	do {									\
		assert(numElts >= 0);						\
		assert(numPointers > 0);					\
		*(word*)frontier = (numElts);					\
		*(word*)(frontier + WORD_SIZE) =				\
			GC_arrayHeader(0, (numPointers));			\
		(dst) = frontier + 2 * WORD_SIZE;				\
/*		fprintf(stderr, "%d  %x = ArrayPointers(%d, %d)\n", __LINE__,*/	\
/*				(dst), (numElts), (numPointers));	     */	\
		frontier = (dst) + ((0 == (numElts)) 				\
			? POINTER_SIZE						\
			: (numElts) * (numPointers) * POINTER_SIZE);		\
		{								\
			word *p;						\
			for (p = (word*) (dst); p < (word*) frontier; ++p)	\
				*p = 0x1;					\
		}								\
	} while (0)

/* ------------------------------------------------- */
/*                       Byte                        */
/* ------------------------------------------------- */

#define Byte_byteToChar(b) b
#define Byte_charToByte(c) c

/* ------------------------------------------------- */
/*                         C                         */
/* ------------------------------------------------- */

#define C_CS_charArrayToWord8Array(x) x

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

#define Int_add(n1, n2) ((n1) + (n2))
#define Int_mul(n1, n2) ((n1) * (n2))
#define Int_sub(n1, n2) (/*fprintf(stderr, "Int_sub(%d, %d) = %d\n", n1, n2, (n1) - (n2)),*/ (n1) - (n2))
int Int_bogus;
#define Int_addCheck(dst, n1, n2, l)				\
	do {							\
		int res;					\
		if (Int_addOverflow(n1, n2, &res)) goto l;	\
		dst = res;					\
	} while (0)
#define Int_mulCheck(dst, n1, n2, l)				\
	do {							\
		int res;					\
		if (Int_mulOverflow(n1, n2, &res)) goto l;	\
		dst = res;					\
	} while (0)
#define Int_negCheck(dst, n, l)					\
	do {							\
		int res;					\
		if (Int_negOverflow(n, &res)) goto l;		\
		dst = res;					\
	} while (0)
#define Int_subCheck(dst, n1, n2, l)				\
	do {							\
		int res;					\
		if (Int_subOverflow(n1, n2, &res)) goto l;	\
		dst = res;					\
	} while (0)
#define checkNew(dst, n1, n2, l, f);						\
	do {									\
		int overflow;							\
		dst = f(n1, n2, &overflow);					\
		if (FALSE)							\
			fprintf(stderr, #f "(%d, %d) = %d\n", n1, n2, dst);	\
		if (overflow) {							\
			if (FALSE)						\
				fprintf(stderr, "overflow\n");			\
			goto l;							\
		}								\
	} while (0)
#define Int_addCheckNew(dst, n1, n2, l)				\
	checkNew(dst, n1, n2, l, Int_addOverflowNew)
#define Int_mulCheckNew(dst, n1, n2, l)				\
	checkNew(dst, n1, n2, l, Int_mulOverflowNew)
#define Int_subCheckNew(dst, n1, n2, l)				\
	checkNew(dst, n1, n2, l, Int_subOverflowNew)
#define Int_negCheckNew(dst, n, l)			\
	do {						\
		int overflow;				\
		dst = Int_negOverflowNew(n, &overflow);	\
		if (overflow) goto l;			\
	} while (0)

#define Int_lt(n1, n2) ((n1) < (n2))
#define Int_le(n1, n2) ((n1) <= (n2))
#define Int_gt(n1, n2) ((n1) > (n2))
#define Int_ge(n1, n2) ((n1) >= (n2))
#define Int_geu(x, y) ((uint)(x) >= (uint)(y))
#define Int_gtu(x, y) ((uint)(x) > (uint)(y))
#define Int_neg(n) (-(n))

/* ------------------------------------------------- */
/*                      IntInf                       */
/* ------------------------------------------------- */

#define IntInf_fromArray(x) x
#define IntInf_toVector(x) x
#define IntInf_toWord(i) ((uint)(i))
#define IntInf_fromWord(w) ((pointer)(w))

/*
 * Check if an IntInf.int is small (i.e., a fixnum).
 */
#define	IntInf_isSmall(arg)						\
	(((uint)(arg) & 0x1) != 0)

/*
 * Check if two IntInf.int's are both small (i.e., fixnums).
 * This is a gross hack, but uses only one test.
 */
#define	IntInf_areSmall(lhs, rhs)					\
	(((uint)(lhs) & (uint)(rhs) & 0x1) != 0)

#define IntInf_add(lhs, rhs, space)	(				\
	intInfRes = IntInf_do_add((lhs), (rhs), (space), frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define IntInf_sub(lhs, rhs, space)	(				\
	intInfRes = IntInf_do_sub((lhs), (rhs), (space), frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define IntInf_toString(arg, base, str) (				\
	intInfRes = IntInf_do_toString(arg, base, str, frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define	IntInf_mul(lhs, rhs, space) (					\
	intInfRes = IntInf_do_mul((lhs), (rhs), (space), frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define IntInf_neg(arg, space) (					\
	intInfRes = IntInf_do_neg(arg, (space), frontier),		\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define	IntInf_quot(num, den, space) (					\
	intInfRes = IntInf_do_quot((num), (den), (space), frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

#define	IntInf_rem(num, den, space) (					\
	intInfRes = IntInf_do_rem((num), (den), (space), frontier),	\
	frontier = intInfRes->frontier,					\
	intInfRes->value)

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

/* Used by polymorphic equality to implement equal on ground types
 * like char, int, word,  and on ref cells.
 * It is emitted by backend/machine.fun.
 */
#define MLton_eq(x, y) ((x) == (y))

#define MLton_halt(frameSize, ret, status)				\
	do {								\
		int x = status;						\
		InvokeRuntime(MLton_exit(x), frameSize, ret);		\
	} while (0)

/* #define MLton_deserialize(z)	(				\ */
/* 	FlushGCExp,						\ */
/* 	deserializeRes = GC_deserialize(&gcState, (z)),			\ */
/* 	CacheGCExp,						\ */
/* 	deserializeRes) */

/* #define MLton_serialize(z)	(				\ */
/* 	FlushGCExp,						\ */
/* 	serializeRes = GC_serialize(&gcState, (z)),			\ */
/* 	CacheGCExp,						\ */
/* 	serializeRes) */

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
#define Real_add(x,y) ((x) + (y))
#define Real_copysign copysign
#define Real_div(x,y) ((x) / (y))
#define Real_equal(x1, x2) ((x1) == (x2))
#define Real_frexp frexp
#define Real_fromInt(n) ((double)(n))
#define Real_ge(x1, x2) ((x1) >= (x2))
#define Real_gt(x1, x2) ((x1) > (x2))
#define Real_ldexp ldexp
#define Real_le(x1, x2) ((x1) <= (x2))
#define Real_lt(x1, x2) ((x1) < (x2))
#define Real_modf modf
#define Real_mul(x,y) ((x) * (y))
#define Real_muladd(x,y,z) ((x) * (y) + (z))
#define Real_mulsub(x,y,z) ((x) * (y) - (z))
#define Real_neg(x) (-(x))
#define Real_sub(x,y) ((x) - (y))
#define Real_toInt(x) ((int)(x))

/* ------------------------------------------------- */
/*                      String                       */
/* ------------------------------------------------- */

#define String_size GC_arrayNumElements
#define String_fromCharVector(x) x
#define String_fromWord8Vector(x) x
#define String_toCharVector(x) x
#define String_toWord8Vector(x) x

/* ------------------------------------------------- */
/*                      Thread                       */
/* ------------------------------------------------- */

#define Thread_copy(frameSize, ret, thread)					\
	do {									\
		pointer t = thread;						\
		InvokeRuntime(GC_copyThread(&gcState, t), frameSize, ret);	\
	} while (0)

#define Thread_copyCurrent(frameSize, ret)					\
	do {									\
		InvokeRuntime(GC_copyCurrentThread(&gcState), frameSize, ret);	\
	} while (0)

#define Thread_finishHandler(frameSize, ret, thread)			\
	do {								\
		GC_thread t = thread;					\
		InvokeRuntime(GC_finishHandler(&gcState, t),		\
					frameSize, ret);		\
	} while (0)

#define Thread_switchTo(frameSize, ret, thread)					\
	do {									\
		GC_thread t = thread;						\
		stackTop += (frameSize);					\
		*(uint*)stackTop = (ret ## _index);				\
	 	gcState.currentThread->stack->used =				\
			stackTop + WORD_SIZE - gcState.stackBottom;		\
	 	gcState.currentThread = t;					\
		gcState.stackBottom =						\
			((pointer)t->stack) + sizeof(struct GC_stack);		\
		stackTop = gcState.stackBottom + t->stack->used - WORD_SIZE;	\
		gcState.stackLimit =						\
			gcState.stackBottom + t->stack->reserved		\
			- 2 * gcState.maxFrameSize;				\
		/* Thread_atomicEnd () */					\
		gcState.canHandle--;						\
		if (gcState.signalIsPending && 0 == gcState.canHandle)		\
			gcState.limit = 0;					\
		/* Thread_atomicEnd () */					\
		Return();							\
		ret:								\
		stackTop -= (frameSize);					\
	} while (0)

/* ------------------------------------------------- */
/*                      Vector                       */
/* ------------------------------------------------- */

#define Vector_length GC_arrayNumElements
#define Vector_fromArray(a) a

/* ------------------------------------------------- */
/*                       Word8                       */
/* ------------------------------------------------- */

#define Word8_add(w1,w2) ((w1) + (w2))
#define Word8_andb(w1,w2) ((w1) & (w2))
/* The macro for Word8_arshift isn't ANSI C, because ANSI doesn't guarantee 
 * sign extension.  We use it anyway cause it always seems to work.
 */
#define Word8_arshift(w, s) ((signed char)(w) >> (s))
/*#define Word8_arshift Word8_arshiftAsm */
#define Word8_div(w1,w2) ((w1) / (w2))
#define Word8_fromInt(x) ((uchar)(x))
#define Word8_fromLargeWord(w) ((uchar)(w))
#define Word8_ge(w1,w2) ((w1) >= (w2))
#define Word8_gt(w1,w2) ((w1) > (w2))
#define Word8_le(w1,w2) ((w1) <= (w2))
#define Word8_lshift(w,s)  ((w) << (s))
#define Word8_lt(w1,w2) ((w1) < (w2))
#define Word8_mod(w1,w2) ((w1) % (w2))
#define Word8_mul(w1,w2) ((w1) * (w2))
#define Word8_neg(w) (-(w))
#define Word8_notb(w) (~(w))
#define Word8_orb(w1,w2) ((w1) | (w2))
#define Word8_ror(x,y) ((x)>>(y) | ((x)<<(8-(y))))
#define Word8_rol(x,y) ((x)>>(8-(y)) | ((x)<<(y)))
#define Word8_rshift(w,s) ((w) >> (s))
#define Word8_sub(w1,w2) ((w1) - (w2))
#define Word8_toInt(w) ((int)(w))
#define Word8_toIntX(x) ((int)(signed char)(x))
#define Word8_toLargeWord(w) ((uint)(w))
#define Word8_toLargeWordX(x) ((uint)(signed char)(x))
#define Word8_xorb(w1,w2) ((w1) ^ (w2))

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
/*#define Word32_arshift Word32_arshiftAsm */
#define Word32_div(w1,w2) ((w1) / (w2))
#define Word32_fromInt(x) ((uint)(x))
#define Word32_ge(w1,w2) ((w1) >= (w2))
#define Word32_gt(w1,w2) ((w1) > (w2))
#define Word32_le(w1,w2) ((w1) <= (w2))
#define Word32_lshift(w,s) ((w) << (s))
#define Word32_lt(w1,w2) ((w1) < (w2))
#define Word32_mod(w1,w2) ((w1) % (w2))
#define Word32_mul(w1,w2) ((w1) * (w2))
#define Word32_neg(w) (-(w))
#define Word32_notb(w) (~(w))
#define Word32_orb(w1,w2) ((w1) | (w2))
#define Word32_ror(x,y) ((x)>>(y) | ((x)<<(32-(y))))
#define Word32_rol(x,y) ((x)>>(32-(y)) | ((x)<<(y)))
#define Word32_rshift(w,s) ((w) >> (s))
#define Word32_sub(w1,w2) ((w1) - (w2))
#define Word32_toIntX(x) ((int)(x))
#define Word32_xorb(w1,w2) ((w1) ^ (w2))

/* ------------------------------------------------- */
/*                       World                       */
/* ------------------------------------------------- */

#define World_save(frameSize, ret, file)					\
	do {									\
		pointer f = (file);						\
		InvokeRuntime(GC_saveWorld(&gcState, f, &saveGlobals),		\
					frameSize, ret);			\
	} while (0)

#endif /* #ifndef _CCODEGEN_H_ */
