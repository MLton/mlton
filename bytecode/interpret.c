#include "interpret.h"
#include "c-chunk.h"

enum {
	DEBUG = TRUE,
};

typedef char* String;

typedef Word32 ArrayIndex;
typedef Word16 DirectIndex;
typedef Word16 GlobalIndex;
typedef Word16 IndirectIndex;
typedef Word32 Label;
typedef WordS16 Offset;  // offset must be signed
typedef Pointer ProgramCounter;
typedef Word16 RegIndex;
typedef Word8 Scale;
typedef Word16 StackOffset;
typedef Pointer StackTop;

//----------------------------------------------------------------------
// Imports
//----------------------------------------------------------------------

void MLton_directCall (DirectIndex i) {}
void MLton_indirectCall (IndirectIndex i) {}

Pointer globalPointer [0];
Pointer PointerReg [0];

#define regs(ty)				\
	int ty##RegI;				\
	ty global##ty[0];			\
	ty ty##VReg[1000];			\
	ty ty##Reg[1000]

regs(Real32);
regs(Real64);
regs(Word8);
regs(Word16);
regs(Word32);
regs(Word64);

#undef regs

//
// Virtual Registers.  Explicitly referenced by the Machine IL.
//

#define R(ty, i) (ty##VReg [i])

//
// Internal Registers.
//

#define PopReg(ty) ty##Reg [ty##RegI--]
#define PushReg(ty) ty##Reg [ty##RegI++]

#define quotRem1(qr, size)						\
	Word##size WordS##size##_##qr (Word##size w1, Word##size w2);
#define quotRem2(qr)				\
	quotRem1 (qr, 8)				\
	quotRem1 (qr, 16)				\
	quotRem1 (qr, 32)				\
	quotRem1 (qr, 64)
quotRem2 (quot)
quotRem2 (rem)
#undef quotRem1
#undef quotRem2

//----------------------------------------------------------------------

#define coercePrims()					\
	coerce (Real32, Real64, Real32, Real64)		\
	coerce (Real32, Word32, Real32, WordS32)	\
	coerce (Real64, Real32, Real64, Real32)		\
	coerce (Real64, Word32, Real64, WordS32)	\
	coerce (Word16, Real32, WordS16, Real32)	\
	coerce (Word16, Real64, WordS16, Real64)	\
	coerce (Word16, Word32, WordS16, Word32)	\
	coerce (Word16, Word64, WordS16, Word64)	\
	coerce (Word32, Real32, WordS32, Real32)	\
	coerce (Word32, Real64, WordS32, Real64)	\
	coerce (Word32, Word64, WordS32, Word64)	\
	coerce (Word8, Real32, WordS8, Real32)		\
	coerce (Word8, Real64, WordS8, Real64)		\
	coerce (Word8, Word16, WordS8, Word16)		\
	coerce (Word8, Word32, WordS8, Word32)		\
	coerce (Word8, Word64, WordS8, Word64)		\
	coerce (Word16, Word32, WordU16, Word32)	\
	coerce (Word16, Word64, WordU16, Word64)	\
	coerce (Word16, Word8, WordU16, Word8)		\
	coerce (Word32, Word16, WordU32, Word16)	\
	coerce (Word32, Word64, WordU32, Word64)	\
	coerce (Word32, Word8, WordU32, Word8)		\
	coerce (Word64, Word16, WordU64, Word16)	\
	coerce (Word64, Word32, WordU64, Word32)	\
	coerce (Word64, Word8, WordU64, Word8)		\
	coerce (Word8, Word16, WordU8, Word16)		\
	coerce (Word8, Word32, WordU8, Word32)		\
	coerce (Word8, Word64, WordU8, Word64)

#define loadStorePrimsOfSize(mode, ty, size)	\
	loadStoreArrayOffset (mode, ty, size)	\
	loadStoreContents (mode, ty, size)	\
	loadStoreGlobal (mode, ty, size)	\
	loadStoreOffset (mode, ty, size)	\
	loadStoreRegister (mode, ty, size)	\
	loadStoreStackOffset (mode, ty, size)

#define loadStorePrims(mode)			\
	loadStorePrimsOfSize (mode, Word, 8)	\
	loadStorePrimsOfSize (mode, Word, 16)	\
	loadStorePrimsOfSize (mode, Word, 32)	\
	loadStorePrimsOfSize (mode, Word, 64)	\
	loadStorePrimsOfSize (mode, Real, 32)	\
	loadStorePrimsOfSize (mode, Real, 64)	\
	loadStoreFrontier (mode)		\
	loadStoreStackTop (mode)

#define realPrimsOfSize(size)				\
	binary (Real##size, Real##size##_add)		\
	binary (Real##size, Real##size##_div)		\
	compare (Real##size, Real##size##_equal)	\
	compare (Real##size, Real##size##_ge)		\
	compare (Real##size, Real##size##_gt)		\
	compare (Real##size, Real##size##_le)		\
	compare (Real##size, Real##size##_lt)		\
	binary (Real##size, Real##size##_mul)		\
	ternary (Real##size, Real##size##_muladd)	\
	ternary (Real##size, Real##size##_mulsub)	\
	unary (Real##size, Real##size##_neg)		\
	unary (Real##size, Real##size##_round)		\
	binary (Real##size, Real##size##_sub)

#define wordPrimsOfSizeNoMul(size)			\
	binary (Word##size, Word##size##_add)		\
	binary (Word##size, Word##size##_andb)		\
	compare (Word##size, Word##size##_equal)	\
	compare (Word##size, WordS##size##_ge)		\
	compare (Word##size, WordU##size##_ge)		\
	compare (Word##size, WordS##size##_gt)		\
	compare (Word##size, WordU##size##_gt)		\
	compare (Word##size, WordS##size##_le)		\
	compare (Word##size, WordU##size##_le)		\
	compare (Word##size, WordS##size##_lt)		\
	compare (Word##size, WordU##size##_lt)		\
	shift (Word##size, Word##size##_lshift)		\
	binary (Word##size, WordS##size##_mul)		\
	binary (Word##size, WordU##size##_mul)		\
	unary (Word##size, Word##size##_neg)		\
	unary (Word##size, Word##size##_notb)		\
	binary (Word##size, Word##size##_orb)		\
	binary (Word##size, WordS##size##_quot)		\
	binary (Word##size, WordU##size##_quot)		\
	binary (Word##size, WordS##size##_rem)		\
	binary (Word##size, WordU##size##_rem)		\
	shift (Word##size, Word##size##_rol)		\
	shift (Word##size, Word##size##_ror)		\
	shift (Word##size, WordS##size##_rshift)	\
	shift (Word##size, WordU##size##_rshift)	\
	binary (Word##size, Word##size##_sub)		\
	binary (Word##size, Word##size##_xorb)		\
	binaryCheck (Word##size, WordS##size##_addCheck)	\
	binaryCheck (Word##size, WordU##size##_addCheck)	\
	unaryCheck (Word##size, Word##size##_negCheck)		\
	binaryCheck (Word##size, WordS##size##_subCheck)	\
	loadStoreConstant (load, Word, size)

#define wordPrimsOfSize(size)					\
	wordPrimsOfSizeNoMul(size)				\
	binaryCheck (Word##size, WordS##size##_mulCheck)	\
	binaryCheck (Word##size, WordU##size##_mulCheck)	\


#define prims()						\
	coercePrims ()					\
	loadGCState ()					\
	loadStorePrims (load)				\
	loadStorePrims (store)				\
	realPrimsOfSize (32)				\
	realPrimsOfSize (64)				\
	wordPrimsOfSize (8)				\
	wordPrimsOfSize (16)				\
	wordPrimsOfSize (32)				\
	wordPrimsOfSizeNoMul (64)

#define opcodes()				\
	prims()					\
	opcodeGen (DirectCall)			\
	opcodeGen (Goto)			\
	opcodeGen (IndirectCall) 		\
	opcodeGen (JumpOnOverflow)		\
	opcodeGen (ProfileLabel)		\
	opcodeGen (Raise)			\
	opcodeGen (Return)			\
	opcodeGen (Switch8)			\
	opcodeGen (Switch16)			\
	opcodeGen (Switch32)			\
	opcodeGen (Switch64)			\
        opcodeGen (Thread_returnToC)

#define opcodeName(ty, size, name) opcodeGen (ty##size##_##name)

#define binary(ty, f)  opcodeGen (f)
#define binaryCheck(ty, f)  opcodeGen (f)
#define compare(ty, f)  opcodeGen (f)
#define loadStoreArrayOffset(mode, ty, size) \
	opcodeName (ty, size, mode##ArrayOffset)
#define	loadStoreContents(mode, ty, size) \
	opcodeName (ty, size, mode##Contents)
#define loadStoreConstant(mode, ty, size) \
	 opcodeName (ty, size, mode##Constant)
#define loadStoreFrontier(mode) opcodeGen (mode##Frontier)
#define loadGCState() opcodeGen (loadGCState)
#define	loadStoreGlobal(mode, ty, size)  opcodeName (ty, size, mode##Global)
#define	loadStoreOffset(mode, ty, size)  opcodeName (ty, size, mode##Offset)
#define	loadStoreRegister(mode, ty, size)  opcodeName (ty, size, mode##Register)
#define	loadStoreStackOffset(mode, ty, size) \
	opcodeName (ty, size, mode##StackOffset)
#define loadStoreStackTop(mode) opcodeGen (mode##StackTop)
#define shift(ty, f)  opcodeGen (f)
#define ternary(ty, f)  opcodeGen (f)
#define unary(ty, f)  opcodeGen (f)
#define unaryCheck(ty, f)  opcodeGen (f)

#define coerceOp(f, t)  opcodeGen (f##_to##t)

#define coerce(f1, t1, f2, t2)  coerceOp (f2, t2)

// Define the opcode strings.

#define opcodeGen(z)  #z,

String opcodeStrings [] = {
	opcodes ()
};

#undef opcodeGen

void MLton_Bytecode_printOpcodes () {
	int i;

	for (i = 0; i < cardof (opcodeStrings); ++i)
		fprintf (stdout, "%s\n", opcodeStrings[i]);
}

// Define the Opcode enum.

#define opcodeSym(z)  OPCODE_##z
#define opcodeSymOfTy(ty, size, z)  opcodeSym(ty##size##_##z)

#define opcodeGen(z) opcodeSym(z),

typedef enum {
	opcodes ()
} Opcode;

#undef opcodeName
#undef coerce
#undef coerceOp
#undef binary
#undef binaryCheck
#undef compare
#undef loadGCState
#undef loadStoreArrayOffset
#undef loadStoreContents
#undef loadStoreConstant
#undef loadStoreFrontier
#undef loadStoreGlobal
#undef loadStoreOffset
#undef loadStoreRegister
#undef loadStoreStackOffset
#undef loadStoreStackTop
#undef shift
#undef ternary
#undef unary
#undef unaryCheck

#define Temp(ty, i) ty##_##i

#define temps(ty)				\
	ty Temp (ty, 0);			\
	ty Temp (ty, 1);			\
	ty Temp (ty, 2)				\

#define Fetch(z)				\
	do {					\
		z = *(typeof(z)*)pc;		\
		pc += sizeof (typeof(z));	\
	} while (0)

enum {
	MODE_load,
	MODE_store,
};

#define loadStore(mode, t, z)			\
	switch (MODE_##mode) {			\
	case MODE_load:				\
		PushReg (t) =  z;		\
	break;					\
	case MODE_store:			\
		z = PopReg (t);			\
	break;					\
	}

#define loadStoreArrayOffset(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##ArrayOffset):			\
		index = PopReg (Word32);					\
		base = (Pointer) (PopReg (Word32));				\
		Fetch (offset);							\
		Fetch (scale);							\
		loadStore (mode, ty##size,					\
				X (ty##size, base, index, offset, scale));	\
	break;

#define loadStoreContents(mode, ty, size)			\
	case opcodeSymOfTy (ty, size, mode##Contents):		\
		base = (Pointer) (PopReg (Word32));		\
		loadStore (mode, ty##size, C (ty##size, base));	\
	break;

#define loadStoreConstant(mode, ty, size)			\
	case opcodeSymOfTy (ty, size, mode##Constant):		\
		Fetch (Temp (ty##size, 0));			\
		loadStore (mode, ty##size, Temp (ty##size, 0));	\
	break;

#define loadStoreFrontier(mode)					\
	case opcodeSym (mode##Frontier):			\
		loadStore (mode, Word32, (Word32)Frontier);	\
	break;

#define loadGCState()						\
	case opcodeSym (loadGCState):				\
		PushReg (Word32) = (Word32)&gcState;		\
	break;

#define loadStoreGlobal(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##Global):			\
		Fetch (globalIndex);					\
		loadStore (mode, ty##size, G (ty##size, globalIndex));	\
	break;

#define loadStoreOffset(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##Offset):			\
		base = (Pointer) (PopReg (Word32));			\
		Fetch (offset);						\
		loadStore (mode, ty##size, O (ty##size, base, offset));	\
	break;

#define loadStoreRegister(mode, ty, size)				\
	case opcodeSymOfTy (ty, size, mode##Register):			\
		Fetch (regIndex);					\
		loadStore (mode, ty##size, R (ty##size, regIndex));	\
	break;

#define loadStoreStackOffset(mode, ty, size)				\
	case opcodeSymOfTy (ty, size, mode##StackOffset):		\
		Fetch (stackOffset);					\
		loadStore (mode, ty##size, S (ty, stackOffset));	\
	break;

#define loadStoreStackTop(mode)					\
	case opcodeSym (mode##StackTop):			\
		loadStore (mode, Word32, (Word32)StackTop);	\
	break;

#define Store(ty, size, x)  StoreZ (ty##size, x)

#define StoreBool(b) b

#define opcode(ty, size, name) OPCODE_##ty##size##_##name

#define coerceOp(f, t) OPCODE_##f##_to##t

#define binary(ty, f)							\
	case opcodeSym (f):						\
		Temp (ty, 0) = PopReg (ty);				\
		Temp (ty, 1) = PopReg (ty);				\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1));		\
	break;

#define binaryCheck(ty, f)							\
	case opcodeSym (f):							\
		Temp (ty, 0) = PopReg (ty);					\
		Temp (ty, 1) = PopReg (ty);					\
		f (PushReg (ty), Temp (ty, 0), Temp (ty, 1), f##Overflow);	\
		overflow = FALSE;						\
		goto f##Continue;						\
	f##Overflow:								\
		overflow = TRUE;						\
	f##Continue:

#define unaryCheck(ty, f)							\
	case opcodeSym (f):							\
		Temp (ty, 0) = PopReg (ty);					\
		f (PushReg (ty), Temp (ty, 0), f##Overflow);			\
		overflow = FALSE;						\
		goto f##Continue;						\
	f##Overflow:								\
		overflow = TRUE;						\
	f##Continue:

#define coerce(f1, t1, f2, t2)					\
	case coerceOp (f2, t2):					\
		PushReg (t1) = f2##_to##t2 (PopReg (f1));	\
	break;

#define compare(ty, f)							\
	case opcodeSym (f):						\
		Temp (ty, 0) = PopReg (ty);				\
		Temp (ty, 1) = PopReg (ty);				\
		PushReg (Word32) = f (Temp (ty, 0), Temp (ty, 1));	\
	break;

#define shift(ty, f)						\
	case opcodeSym (f):					\
		Temp (ty, 0) = PopReg (ty);			\
		Temp (Word32, 1) = PopReg (Word32);		\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1));	\
	break;

#define ternary(ty, f)								\
	case opcodeSym (f):							\
		Temp (ty, 0) = PopReg (ty);					\
		Temp (ty, 1) = PopReg (ty);					\
		Temp (ty, 2) = PopReg (ty);					\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1), Temp (ty, 2));	\
	break;

#define unary(ty, f)					\
	case opcodeSym (f):				\
		Temp (ty, 0) = PopReg (ty);		\
		PushReg (ty) = f (Temp (ty, 0));	\
	break;

#define ExnStackOffset offsetof (struct GC_state, exnStack)
#define FrontierOffset offsetof (struct GC_state, frontier)
#define StackBottomOffset offsetof (struct GC_state, stackBottom)
#define StackTopOffset offsetof (struct GC_state, stackTop)

struct GC_state gcState;

#define Goto(l)					\
	do {					\
		pc = code + l;			\
		goto mainLoop;			\
	} while (0)

#define Switch(size)						\
	case OPCODE_Switch##size:				\
		caseTest##size = PopReg (Word##size);		\
		Fetch (numCases);				\
		lastCase = pc + (4 + size/8) * numCases;	\
		while (pc < lastCase) {				\
			Fetch (caseWord##size);			\
			Fetch (label);				\
			if (caseTest##size == caseWord##size)	\
				Goto (label);			\
		}						\
		/* Default case. */				\
		Fetch (label);					\
		Goto (label);

void MLton_Bytecode_interpret (Pointer code, Word32 codeOffset) {
	Pointer base;
	Word8 caseTest8;
	Word16 caseTest16;
	Word32 caseTest32;
	Word64 caseTest64;
	Word8 caseWord8;
	Word16 caseWord16;
	Word32 caseWord32;
	Word64 caseWord64;
	DirectIndex directIndex;
	Pointer frontier;
	GlobalIndex globalIndex;
	ArrayIndex index;
	IndirectIndex indirectIndex;
	Label label;
	ProgramCounter lastCase;
	Word16 numCases;
	Offset offset;
	Opcode opc;
	Bool overflow;
	ProgramCounter pc;
	RegIndex regIndex;
	Scale scale;
	StackOffset stackOffset;
	StackTop stackTop;
	temps (Real32);
	temps (Real64);
	temps (Word8);
	temps (Word16);
	temps (Word32);
	temps (Word64);

	// Quell unused variable warnings.
	Word8_2 = 0;
	Word16_2 = 0;
	Word32_2 = 0;
	Word64_2 = 0;

	pc = code + codeOffset;
mainLoop:
	Fetch (opc);
	if (DEBUG)
		fprintf (stderr, "opc = %s  pc = %d\n",
				opcodeStrings[opc],
				pc - code);
	switch (opc) {
	prims ();
 	case opcodeSym (DirectCall):
 		Fetch (directIndex);
 		MLton_directCall (directIndex);
 		goto mainLoop;
 	case opcodeSym (Goto):
		Fetch (label);
 		Goto (label);
 	case opcodeSym (IndirectCall):
 		Fetch (indirectIndex);
 		MLton_indirectCall (indirectIndex);
 		goto mainLoop;
	case opcodeSym (JumpOnOverflow):
		Fetch (label);
		if (overflow)
			Goto (label);
		goto mainLoop;
 	case opcodeSym (ProfileLabel):
 		die ("ProfileLabel not implemented");
 	case opcodeSym (Raise):
 		StackTop = StackBottom + ExnStack;
		// fall through to Return
 	case opcodeSym (Return):
		Goto (*(Label*)(StackTop - sizeof(Label)));
	Switch(8);
	Switch(16);
	Switch(32);
	Switch(64);
 	case opcodeSym (Thread_returnToC):
 		return;
	}
	goto mainLoop;
}

