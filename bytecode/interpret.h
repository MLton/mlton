#ifndef _INTERPRET_H_
#define _INTERPRET_H_

#include <stdio.h>
#include "types.h"
#include "assert.h"

#define regs(ty)				\
	extern int ty##RegI;			\
	extern ty ty##Reg[]

regs(Real32);
regs(Real64);
regs(Word8);
regs(Word16);
regs(Word32);
regs(Word64);

#undef regs

#define assertRegsEmpty()			\
	do {					\
		assert (0 == Word8RegI);	\
		assert (0 == Word16RegI);	\
		assert (0 == Word32RegI);	\
		assert (0 == Word64RegI);	\
		assert (0 == Real32RegI);	\
		assert (0 == Real64RegI);	\
	} while (0)

struct NameOffsets {
	Word32 codeOffset;  // An offset into code.
	Word32 nameOffset;  // An offset into addressNames.
};

typedef struct Bytecode {
	char *addressNames;
	Pointer code;
	Word32 codeSize;
	struct NameOffsets *nameOffsets;
	Word32 nameOffsetsSize;
} *Bytecode;

#define PopReg(ty) (assert (ty##RegI > 0), ty##Reg [--ty##RegI])
#define PushReg(ty) ty##Reg [ty##RegI++]

void MLton_callC (int i);  // provided by client
void MLton_Bytecode_interpret (Bytecode b, Word32 codeOffset);

#endif
