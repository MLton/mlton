#include "interpret.h"
#include "platform.h"
#include "c-chunk.h"	// c-chunk.h must come before opcode.h because it
			// redefines some opcode symbols
#include "opcode.h"

enum {
	DEBUG = TRUE,
};

typedef Word32 ArrayIndex;
typedef Word16 CallCIndex;
typedef Word16 GlobalIndex;
typedef Word32 Label;
typedef WordS16 Offset;  // offset must be signed
typedef Pointer ProgramCounter;
typedef Word16 RegIndex;
typedef Word8 Scale;
typedef Word16 StackOffset;
typedef Pointer StackTop;

struct GC_state gcState;

//----------------------------------------------------------------------
// Imports
//----------------------------------------------------------------------

#define regs(ty)				\
	int ty##RegI;				\
	ty global##ty[0];			\
	static ty ty##VReg[1000];		\
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

#define loadWord(size)							\
	case opcodeSymOfTy (Word, size, loadWord):			\
		Fetch (Temp (Word##size, 0));				\
		loadStore (load, Word##size, Temp (Word##size, 0));	\
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
	CallCIndex callCIndex;
	Pointer frontier;
	GlobalIndex globalIndex;
	ArrayIndex index;
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
 	case opcodeSym (CallC):
 		Fetch (callCIndex);
		gcState.frontier = frontier;
		gcState.stackTop = stackTop;
 		MLton_callC (callCIndex);
		frontier = gcState.frontier;
		stackTop = gcState.stackTop;
		goto doReturn;
 	case opcodeSym (Goto):
		Fetch (label);
 		Goto (label);
	case opcodeSym (JumpOnOverflow):
		Fetch (label);
		if (overflow)
			Goto (label);
		goto mainLoop;
 	case opcodeSym (ProfileLabel):
 		die ("ProfileLabel not implemented");
 	case opcodeSym (Raise):
 		stackTop = gcState.stackBottom + gcState.exnStack;
		goto doReturn;
 	case opcodeSym (Return):
doReturn:
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

