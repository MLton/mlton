#include "interpret.h"
#include "platform.h"
#include "c-chunk.h"	// c-chunk.h must come before opcode.h because it
			// redefines some opcode symbols
#include "opcode.h"

enum {
	DEBUG = TRUE,
};

typedef Word32 ArrayIndex;
typedef Word16 ArrayOffset;
typedef Word16 CallCIndex;
typedef Word16 GlobalIndex;
typedef Word32 Label;
typedef WordS16 Offset;  // Offset must be signed.
typedef Pointer ProgramCounter;
typedef Word16 RegIndex;
typedef Word8 Scale;
typedef WordS16 StackOffset;  // StackOffset must be signed.
typedef Pointer StackTop;

struct GC_state gcState;

//----------------------------------------------------------------------
// Imports
//----------------------------------------------------------------------

#define regs(ty)				\
	int ty##RegI = 0;			\
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

#define Fetch(z)								\
	do {									\
		z = *(typeof(z)*)pc;						\
		if (DEBUG or disassemble) {					\
 			if (#z == "label")					\
				fprintf (stderr, " %s", offsetToLabel[z]);	\
			else if (#z != "opc")					\
				fprintf (stderr, " %d", (int)z);		\
		}								\
 		pc += sizeof (typeof(z));					\
	} while (0)

enum {
	MODE_load,
	MODE_store,
};

#define maybe unless (disassemble)

#define StoreReg(t, z) maybe PushReg(t) = z

#define loadStore(mode, t, z)			\
	switch (MODE_##mode) {			\
	case MODE_load:				\
		StoreReg (t, z);		\
	break;					\
	case MODE_store:			\
		maybe z = PopReg (t);		\
	break;					\
	}

#define loadStoreArrayOffset(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##ArrayOffset):			\
		unless (disassemble) {						\
			index = PopReg (Word32);				\
			base = (Pointer) (PopReg (Word32));			\
		}								\
		Fetch (arrayOffset);						\
		Fetch (scale);							\
		loadStore (mode, ty##size,					\
				X (ty##size, base, index, offset, scale));	\
	goto mainLoop;

#define loadStoreContents(mode, ty, size)			\
	case opcodeSymOfTy (ty, size, mode##Contents):		\
		maybe base = (Pointer) (PopReg (Word32));	\
		loadStore (mode, ty##size, C (ty##size, base));	\
	goto mainLoop;

#define loadWord(size)							\
	case opcodeSymOfTy (Word, size, loadWord):			\
		Fetch (Temp (Word##size, 0));				\
		loadStore (load, Word##size, Temp (Word##size, 0));	\
	goto mainLoop;

#define loadStoreFrontier(mode)					\
	case opcodeSym (mode##Frontier):			\
		loadStore (mode, Word32, (Word32)Frontier);	\
	goto mainLoop;

#define loadGCState()					\
	case opcodeSym (loadGCState):			\
		StoreReg (Word32, (Word32)&gcState);	\
	goto mainLoop;

#define loadStoreGlobal(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##Global):			\
		Fetch (globalIndex);					\
		loadStore (mode, ty##size, G (ty##size, globalIndex));	\
	goto mainLoop;

#define loadStoreOffset(mode, ty, size)					\
	case opcodeSymOfTy (ty, size, mode##Offset):			\
		maybe base = (Pointer) (PopReg (Word32));		\
		Fetch (offset);						\
		loadStore (mode, ty##size, O (ty##size, base, offset));	\
	goto mainLoop;

#define loadStoreRegister(mode, ty, size)				\
	case opcodeSymOfTy (ty, size, mode##Register):			\
		Fetch (regIndex);					\
		loadStore (mode, ty##size, R (ty##size, regIndex));	\
	goto mainLoop;

#define loadStoreStackOffset(mode, ty, size)				\
	case opcodeSymOfTy (ty, size, mode##StackOffset):		\
		Fetch (stackOffset);					\
		loadStore (mode, ty##size, S (ty, stackOffset));	\
	goto mainLoop;

#define loadStoreStackTop(mode)					\
	case opcodeSym (mode##StackTop):			\
		loadStore (mode, Word32, (Word32)StackTop);	\
	goto mainLoop;

#define opcode(ty, size, name) OPCODE_##ty##size##_##name

#define coerceOp(f, t) OPCODE_##f##_to##t

#define binary(ty, f)						\
	case opcodeSym (f):					\
		if (disassemble) goto mainLoop;			\
		Temp (ty, 0) = PopReg (ty);			\
		Temp (ty, 1) = PopReg (ty);			\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1));	\
	goto mainLoop;

#define binaryCheck(ty, f)							\
	case opcodeSym (f):							\
		if (disassemble) goto mainLoop;					\
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
		if (disassemble) goto mainLoop;					\
		Temp (ty, 0) = PopReg (ty);					\
		f (PushReg (ty), Temp (ty, 0), f##Overflow);			\
		overflow = FALSE;						\
		goto f##Continue;						\
	f##Overflow:								\
		overflow = TRUE;						\
	f##Continue:

#define coerce(f1, t1, f2, t2)					\
	case coerceOp (f2, t2):					\
		if (disassemble) goto mainLoop;			\
		PushReg (t1) = f2##_to##t2 (PopReg (f1));	\
	goto mainLoop;

#define compare(ty, f)							\
	case opcodeSym (f):						\
		if (disassemble) goto mainLoop;				\
		Temp (ty, 0) = PopReg (ty);				\
		Temp (ty, 1) = PopReg (ty);				\
		PushReg (Word32) = f (Temp (ty, 0), Temp (ty, 1));	\
	goto mainLoop;

#define shift(ty, f)						\
	case opcodeSym (f):					\
		if (disassemble) goto mainLoop;			\
		Temp (ty, 0) = PopReg (ty);			\
		Temp (Word32, 1) = PopReg (Word32);		\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1));	\
	goto mainLoop;

#define ternary(ty, f)								\
	case opcodeSym (f):							\
		if (disassemble) goto mainLoop;					\
		Temp (ty, 0) = PopReg (ty);					\
		Temp (ty, 1) = PopReg (ty);					\
		Temp (ty, 2) = PopReg (ty);					\
		PushReg (ty) = f (Temp (ty, 0), Temp (ty, 1), Temp (ty, 2));	\
	goto mainLoop;

#define unary(ty, f)					\
	case opcodeSym (f):				\
		if (disassemble) goto mainLoop;		\
		Temp (ty, 0) = PopReg (ty);		\
		PushReg (ty) = f (Temp (ty, 0));	\
	goto mainLoop;

#define Goto(l)					\
	do {					\
		maybe pc = code + l;		\
		goto mainLoop;			\
	} while (0)

#define Switch(size)							\
	case OPCODE_Switch##size:					\
		caseTest##size = PopReg (Word##size);			\
		Fetch (numCases);					\
		lastCase = pc + (4 + size/8) * numCases;		\
		while (pc < lastCase) {					\
			if (disassemble)				\
				fprintf (stderr, "\n\t  ");		\
			Fetch (caseWord##size);				\
			if (disassemble)				\
				fprintf (stderr, " =>");		\
			Fetch (label);					\
			if (not disassemble 				\
				and caseTest##size == caseWord##size)	\
				Goto (label);				\
		}							\
		fprintf (stderr, "\n\t   _ =>");			\
       		/* Default case. */					\
		Fetch (label);						\
		Goto (label);

typedef char *String;

static inline void interpret (Bytecode b, Word32 codeOffset, Bool disassemble) {
	int addressNameIndex;
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
	Pointer code;
	Pointer frontier;
	GlobalIndex globalIndex;
	int i;
	ArrayIndex index;
	ArrayOffset arrayOffset;
	Label label;
	ProgramCounter lastCase;
	String name;
	Word16 numCases;
	Offset offset;
	String *offsetToLabel;
	Opcode opc;
	Bool overflow;
	ProgramCounter pc;
	ProgramCounter pcMax;
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

	code = b->code;
	pcMax = b->code + b->codeSize;
	if (DEBUG or disassemble) {
		addressNameIndex = 0;
		ARRAY (offsetToLabel, b->codeSize);
		for (i = 0; i < b->addressNamesSize; ++i)
			offsetToLabel [b->addressNames[i].offset] =
				b->addressNames[i].name;
	}
	if (disassemble)
		pc = code;
	else
		pc = code + codeOffset;
mainLoop:
	if (DEBUG or disassemble) {
		if (pc == pcMax)
			goto done;
		offset = pc - b->code;
		name = offsetToLabel [offset];
		unless (NULL == name)
			fprintf (stderr, "\n%s:", name);
		fprintf (stderr, "\n\t");
	}
	assert (code <= pc and pc < pcMax);
	Fetch (opc);
	assert (opc < cardof (opcodeStrings));
	if (DEBUG or disassemble)
		fprintf (stderr, "%s", opcodeStrings[opc]);
	switch (opc) {
	prims ();
 	case opcodeSym (CallC):
 		Fetch (callCIndex);
		unless (disassemble) {
			gcState.frontier = frontier;
			gcState.stackTop = stackTop;
 			MLton_callC (callCIndex);
			frontier = gcState.frontier;
			stackTop = gcState.stackTop;
		}
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
		unless (disassemble)
			stackTop = gcState.stackBottom + gcState.exnStack;
		goto doReturn;
 	case opcodeSym (Return):
doReturn:
		Goto (*(Label*)(StackTop - sizeof (Label)));
	Switch(8);
	Switch(16);
	Switch(32);
	Switch(64);
 	case opcodeSym (Thread_returnToC):
 		maybe goto done;
	}
	assert (FALSE);
	// Quell unused variable warnings.
	if (FALSE) {
		Word8_2 = 0;
		Word16_2 = 0;
		Word32_2 = 0;
		Word64_2 = 0;
	}
done:
	if (DEBUG or disassemble)
		free (offsetToLabel);
	return;
}

static void disassemble (Bytecode b, Word32 codeOffset) {
	interpret (b, codeOffset, TRUE);
	fprintf (stderr, "\n");
}

void MLton_Bytecode_interpret (Bytecode b, Word32 codeOffset) {
	if (DEBUG) {
		fprintf (stderr, "MLton_Bytecode_interpret (0x%08x, %u)\n",
				(uint)b,
				(uint)codeOffset);
		disassemble (b, codeOffset);
		fprintf (stderr, "interpret starting\n");
	}
	interpret (b, codeOffset, FALSE);
}
