#include <stddef.h>
#include "gc.h"
#include "c-chunk.h"
#include "interpret.h"

enum {
	DEBUG = TRUE,
};

typedef char* String;

//----------------------------------------------------------------------
// Imports
//----------------------------------------------------------------------

extern void callCFunction (Word16 f);

void callCFunction (Word16 f) {
	die ("callCFunction unimplemented");
}

Pointer globalPointer [0];
Pointer PointerReg [0];

Real32 globalReal32 [0];
Real64 globalReal64 [0];

Real32 Real32Reg [0];
Real64 Real64Reg [0];

#define words(size)				\
	Word##size globalWord##size [0];	\
	Word##size globalWordS##size [0];	\
	Word##size globalWordU##size [0];	\
	Word##size Word##size##Reg [0];		\
	Word##size WordS##size##Reg [0];	\
	Word##size WordU##size##Reg [0]

words(8);
words(16);
words(32);
words(64);

#undef words

//----------------------------------------------------------------------

#define coercePrims()				\
	coerce (Real32, Real64)			\
	coerce (Real32, WordS32)		\
	coerce (Real64, Real32)			\
	coerce (Real64, WordS32)		\
	coerce (WordS16, Real32)		\
	coerce (WordS16, Real64)		\
	coerce (WordS16, Word32)		\
	coerce (WordS16, Word64)		\
	coerce (WordS32, Real32)		\
	coerce (WordS32, Real64)		\
	coerce (WordS32, Word64)		\
	coerce (WordS8, Real32)			\
	coerce (WordS8, Real64)			\
	coerce (WordS8, Word16)			\
	coerce (WordS8, Word32)			\
	coerce (WordS8, Word64)			\
	coerce (WordU16, Word32)		\
	coerce (WordU16, Word64)		\
	coerce (WordU16, Word8)			\
	coerce (WordU32, Word16)		\
	coerce (WordU32, Word64)		\
	coerce (WordU32, Word8)			\
	coerce (WordU64, Word16)		\
	coerce (WordU64, Word32)		\
	coerce (WordU64, Word8)			\
	coerce (WordU8, Word16)			\
	coerce (WordU8, Word32)			\
	coerce (WordU8, Word64)

#define realPrims(size)				\
	binary (Real, size, add)		\
	binary (Real, size, div)		\
	compare (Real, size, equal)		\
	compare (Real, size, ge)		\
	compare (Real, size, gt)		\
	compare (Real, size, le)		\
	compare (Real, size, lt)		\
	binary (Real, size, mul)		\
	ternary (Real, size, muladd)		\
	ternary (Real, size, mulsub)		\
	unary (Real, size, neg)			\
	unary (Real, size, round)		\
	binary (Real, size, sub)

#define wordPrims(size)				\
	binary (Word, size, add)		\
	binary (Word, size, andb)		\
	compare (Word, size, equal)		\
	compare (Word, S##size, ge)		\
	compare (Word, U##size, ge)		\
	compare (Word, S##size, gt)		\
	compare (Word, U##size, gt)		\
	compare (Word, S##size, le)		\
	compare (Word, U##size, le)		\
	compare (Word, S##size, lt)		\
	compare (Word, U##size, lt)		\
	shift (Word, size, lshift)		\
	binary (Word, S##size, mul)		\
	binary (Word, U##size, mul)		\
	unary (Word, size, neg)			\
	unary (Word, size, notb)		\
	binary (Word, size, orb)		\
	binary (Word, S##size, quot)		\
	binary (Word, U##size, quot)		\
	binary (Word, S##size, rem)		\
	binary (Word, U##size, rem)		\
	shift (Word, size, rol)			\
	shift (Word, size, ror)			\
	shift (Word, S##size, rshift)		\
	shift (Word, U##size, rshift)		\
	binary (Word, size, sub)		\
	binary (Word, size, xorb)

#define moveOpcode(t)				\
	OPCODE_##t##_move

#define moves()					\
	move (Real32)				\
	move (Real64)				\
	move (Word8)				\
	move (Word16)				\
	move (Word32)				\
	move (Word64)

#define opcodes()								\
	moves ()								\
	coercePrims ()								\
	realPrims (32)								\
	realPrims (64)								\
	wordPrims (8)								\
	wordPrims (16)								\
	wordPrims (32)								\
	wordPrims (64)								\
	opcodeGen (CacheFrontier)						\
	opcodeGen (CacheStackTop)						\
	opcodeGen (Call)							\
	opcodeGen (CCall)							\
	opcodeGen (FlushFrontier)						\
	opcodeGen (FlushStackTop)						\
	opcodeGen (Goto)							\
	opcodeGen (Object)							\
	opcodeGen (ProfileLabel)						\
	opcodeGen (Raise)							\
	opcodeGen (Return)							\
	opcodeGen (Switch8)							\
	opcodeGen (Switch16)							\
	opcodeGen (Switch32)							\
	opcodeGen (Switch64)							\
        opcodeGen (Thread_returnToC)						\
	opcodeGen (ArrayOffset)							\
	opcodeGen (Contents)							\
	opcodeGen (Frontier)							\
	opcodeGen (GCState)							\
	opcodeGen (Global)							\
	opcodeGen (Offset)							\
	opcodeGen (Register)							\
	opcodeGen (StackOffset)							\
	opcodeGen (StackTop)							\
	opcodeGen (Word)

#define opcodeName(ty, size, name) opcodeGen (ty##size##_##name)

#define binary(ty, size, name)  opcodeName (ty, size, name)
#define compare(ty, size, name)  opcodeName (ty, size, name)
#define move(ty)  opcodeGen (ty##_move)
#define shift(ty, size, name)  opcodeName (ty, size, name)
#define ternary(ty, size, name)  opcodeName (ty, size, name)
#define unary(ty, size, name)  opcodeName (ty, size, name)

#define coerceOp(f, t)  opcodeGen (f##_to##t)

#define coerce(f, t) coerceOp (f, t)

// Define the opcode strings.

#define opcodeGen(z)  #z,

String opcodeStrings [] = {
	opcodes ()
};

#undef opcodeGen

// Define the Opcode enum.

#define opcodeGen(z) OPCODE_##z,

typedef enum {
	opcodes ()
} Opcode;

typedef Opcode OperandCode;

#undef coerce
#undef coerceOp
#undef binary
#undef compare
#undef move
#undef opcodeName
#undef shift
#undef ternary
#undef unary

#define regZ(ty, i) ty##_##i

#define reg(ty, size, i) regZ (ty##size, i)

#define Fetch(t, z)				\
	do {					\
		z = *(t*)pc;			\
		pc += sizeof(t);		\
	} while (0)

#define LoadSimple(ty, x)			\
	Fetch (Word8, operandCode);		\
	switch (operandCode) {			\
	case OPCODE_Frontier:			\
		x = (typeof(x))Frontier;	\
	break;					\
	case OPCODE_Global:			\
		Fetch (Word16, globalIndex);	\
		x = G (ty, globalIndex);	\
	break;					\
	case OPCODE_Register:			\
		Fetch (Word16, regIndex);	\
		x = ty##Reg[regIndex];		\
	break;					\
	case OPCODE_StackOffset:		\
		Fetch (Word16, stackOffset);	\
		x = S (ty, stackOffset);	\
	break;					\
	case OPCODE_Word:			\
		Fetch (ty, x);			\
	break;					\
	}

#define LoadZ(ty, i)						\
	Fetch (Word8, operandCode);				\
	switch (operandCode) {					\
	case OPCODE_ArrayOffset:				\
		LoadSimple (Pointer, base);			\
		LoadSimple (Word32, arrayIndex);		\
		regZ (ty, i) = X (ty, base, arrayIndex);	\
	break;							\
	case OPCODE_Contents:					\
		LoadSimple (Pointer, base);			\
		regZ (ty, i) = C (ty, base);			\
	break;							\
	case OPCODE_Frontier:					\
		regZ (ty, i) = (ty)(Word32)Frontier;		\
	break;							\
	case OPCODE_Global:					\
		Fetch (Word16, globalIndex);			\
		regZ (ty, i) = G (ty, globalIndex);		\
	break;							\
	case OPCODE_Offset:					\
		LoadSimple (Pointer, base);			\
		Fetch (Word16, offset);				\
		regZ (ty, i) = O (ty, base, (WordS16)offset);	\
	break;							\
	case OPCODE_Register:					\
		Fetch (Word16, regIndex);			\
		regZ (ty, i) = ty##Reg[regIndex];		\
	break;							\
	case OPCODE_StackOffset:				\
		Fetch (Word16, stackOffset);			\
		regZ (ty, i) = S (ty, stackOffset);		\
	break;							\
	case OPCODE_StackTop:					\
		regZ (ty, i) = (ty)(Word32)StackTop;		\
	break;							\
	case OPCODE_Word:					\
		Fetch (ty, regZ (ty, i));			\
	break;							\
	}

#define Load(ty, size, i) LoadZ (ty##size, i)

#define StoreZ(ty, x)						\
	Fetch (Word8, operandCode);				\
	switch (operandCode) {					\
	case OPCODE_ArrayOffset:				\
		LoadSimple (Pointer, base);			\
		LoadSimple (Word32, arrayIndex);		\
		X (ty, base, arrayIndex) = x;			\
	break;							\
	case OPCODE_Contents:					\
		LoadSimple (Pointer, base);			\
		C (ty, base) = x;				\
	break;							\
	case OPCODE_Frontier:					\
		Frontier = (Pointer)(Word32)x;			\
	break;							\
	case OPCODE_Global:					\
		Fetch (Word16, globalIndex);			\
		G (ty, globalIndex) = x;			\
	break;							\
	case OPCODE_Offset:					\
		LoadSimple (Pointer, base);			\
		Fetch (Word16, offset);				\
		O (ty, base, (WordS16)offset) = x;		\
	break;							\
	case OPCODE_Register:					\
		Fetch (Word16, regIndex);			\
		ty##Reg[regIndex] = x;				\
	break;							\
	case OPCODE_StackOffset:				\
		Fetch (Word16, stackOffset);			\
		S (ty, stackOffset) = x;			\
	break;							\
	case OPCODE_StackTop:					\
		StackTop = (Pointer)(Word32)x;			\
	break;							\
	}

#define Store(ty, size, x)  StoreZ (ty##size, x)

#define StoreBool(b) b

#define opcode(ty, size, name) OPCODE_##ty##size##_##name

#define coerceOp(f, t) OPCODE_##f##_to##t

#define binary(ty, size, name)							\
	case opcode (ty, size, name):						\
		Load (ty, size, 0);						\
		Load (ty, size, 1);						\
		Store (ty, size, ty##size##_##name (reg (ty, size, 0),		\
							reg (ty, size, 1)));	\
	break;

#define coerce(f, t)					\
	case coerceOp (f, t):				\
		LoadZ (f, 0);				\
		StoreZ (t, f##_to##t (regZ (f, 0)));	\
	break;

#define compare(ty, size, name)						\
	case opcode (ty, size, name):					\
		Load (ty, size, 0);					\
		Load (ty, size, 1);					\
		StoreBool (ty##size##_##name (reg (ty, size, 0),	\
						reg (ty, size, 1)));	\
	break;

#define shift(ty, size, name)						\
	case opcode (ty, size, name):					\
		Load (ty, size, 0);					\
		Load (ty, 32, 1);					\
		Store (ty, size,					\
			ty##size##_##name (reg (ty, size, 0),		\
						reg (ty, 32, 1)));	\
	break;

#define move(ty)				\
	case moveOpcode (ty):			\
		LoadZ (ty, 0);			\
		StoreZ (ty, regZ (ty, 0));	\
	break;

#define ternary(ty, size, name)						\
	case opcode (ty, size, name):					\
		Load (ty, size, 0);					\
		Load (ty, size, 1);					\
		Load (ty, size, 2);					\
		Store (ty, size, 					\
			ty##size##_##name (reg (ty, size, 0),		\
						reg (ty, size, 1),	\
						reg (ty, size, 2)));	\
	break;

#define unary(ty, size, name)							\
	case opcode (ty, size, name):						\
		Load (ty, size, 0);						\
		Store (ty, size, ty##size##_##name (reg (ty, size, 0)));	\
	break;

#define regs(ty, var)				\
	ty regZ (ty, 0), regZ (ty, 1), regZ (ty, 2)

#define realRegs(size)				\
	regs (Real##size, r##size)

#define wordRegs(size)				\
	regs (Word##size, w##size);		\
	regs (WordS##size, wS##size);		\
	regs (WordU##size, wU##size)

#define ExnStackOffset offsetof (struct GC_state, exnStack)
#define FrontierOffset offsetof (struct GC_state, frontier)
#define StackBottomOffset offsetof (struct GC_state, stackBottom)
#define StackTopOffset offsetof (struct GC_state, stackTop)

struct GC_state gcState;

#define GOTO()					\
	do {					\
		Fetch (Word32, codeOffset);	\
		pc = code + codeOffset;		\
		goto mainLoop;			\
	} while (0)


#define RETURN()						\
	do {							\
		codeOffset = *(Word*)(StackTop - WORD_SIZE);	\
		pc = code + codeOffset;				\
		goto mainLoop;					\
	} while (0)

#define SWITCH(size)						\
	case OPCODE_Switch##size:				\
		Fetch (Word##size, caseTest##size);		\
		Fetch (Word16, numCases);			\
		lastCase = pc + (4 + size/8) * numCases;	\
		while (pc < lastCase) {				\
			FetchZ (Word##size, caseWord##size);	\
			if (caseTest##size == caseWord##size)	\
				GOTO();				\
			pc += 4;				\
		}						\
		/* Default case. */				\
		GOTO();

void MLton_Bytecode_interpret (Pointer code, Word32 codeOffset) {
	Word32 arrayIndex;
	Pointer base;
	Word8 caseTest8;
	Word16 caseTest16;
	Word32 caseTest32;
	Word64 caseTest64;
	Word8 caseWord8;
	Word16 caseWord16;
	Word32 caseWord32;
	Word64 caseWord64;
	Word16 cFunc;
	Word16 frameSize;
	Pointer frontier;
	Word16 globalIndex;
	Word32 header;
	Word16 objectSize;
	Word16 offset;
	Opcode opc;
	OperandCode operandCode;
	Pointer pc;
	Word16 regIndex;
	Word32 returnAddress;
	Word16 stackOffset;
	Pointer stackTop;
	realRegs (32);
	realRegs (64);
	wordRegs (8);
	wordRegs (16);
	wordRegs (32);
	wordRegs (64);

	pc = code + codeOffset;
mainLoop:
	Fetch (Word8, opc);
	if (DEBUG)
		fprintf (stderr, "opc = %s  pc = %d\n",
				opcodeStrings[opc],
				pc - code);
	switch (opc) {
	coercePrims ();
	moves ();
	realPrims (32);
	realPrims (64);
	wordPrims (8);
	wordPrims (16);
	wordPrims (32);
	wordPrims (64);
	case OPCODE_CacheFrontier:
		CacheFrontier();
		goto mainLoop;
	case OPCODE_CacheStackTop:
		CacheStackTop();
		goto mainLoop;
	case OPCODE_Call:
		Fetch (Word16, frameSize);
		Fetch (Word32, returnAddress);
		stackTop += frameSize;
		S(Word32, -4) = returnAddress;
		GOTO ();
	case OPCODE_CCall:
		Fetch (Word16, cFunc);
		callCFunction (cFunc);
		goto mainLoop;
	case OPCODE_FlushFrontier:
		FlushFrontier();
		goto mainLoop;
	case OPCODE_FlushStackTop:
		FlushStackTop();
		goto mainLoop;
	case OPCODE_Goto:
		GOTO ();
	case OPCODE_Object:
		Fetch (Word32, header);
		*(Word32*)Frontier = header;
		StoreZ (Word32, header + 4);
		Fetch (Word16, objectSize);
		Frontier += objectSize;
		goto mainLoop;
	case OPCODE_ProfileLabel:
		die ("ProfileLabel not implemented");
	case OPCODE_Raise:
		StackTop = StackBottom + ExnStack;
		RETURN();
	case OPCODE_Return:
		RETURN();
	case OPCODE_Thread_returnToC:
		return;
	}
	goto mainLoop;
}

void MLton_Bytecode_printOpcodes () {
	int i;

	for (i = 0; i < cardof (opcodeStrings); ++i)
		fprintf (stdout, "%s\n", opcodeStrings[i]);
}
