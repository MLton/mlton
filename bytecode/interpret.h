#ifndef _INTERPRET_H_
#define _INTERPRET_H_

#include <stdio.h>
#include "types.h"

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

struct AddressName {
	char *name;
	Word32 offset;
};

typedef struct Bytecode {
	struct AddressName *addressNames;
	Word32 addressNamesSize;
	Pointer code;
	Word32 codeSize;
} *Bytecode;

#define PopReg(ty) ty##Reg [--ty##RegI]
#define PushReg(ty) ty##Reg [ty##RegI++]

void MLton_callC (int i);  // provided by client
void MLton_Bytecode_interpret (Bytecode b, Word32 codeOffset);

#endif
