#ifndef _INTERPRET_H_
#define _INTERPRET_H_

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

#define PopReg(ty) ty##Reg [ty##RegI--]
#define PushReg(ty) ty##Reg [ty##RegI++]

void MLton_callC (int i);  // provided by client
void MLton_Bytecode_interpret (Pointer code, Word32 codeOffset);

#endif
