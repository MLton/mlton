#ifndef _INTERPRET_H_
#define _INTERPRET_H_

#include "types.h"

void MLton_Bytecode_interpret (Pointer code, Word32 codeOffset);
void MLton_Bytecode_printOpcodes ();

#endif
