/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _INTERPRET_H_
#define _INTERPRET_H_

#include <stdio.h>
#include "assert.h"

#define regs(ty)                                \
        extern int ty##RegI;                    \
        extern ty ty##Reg[]

regs(CPointer);
regs(Objptr);
regs(Real32);
regs(Real64);
regs(Word8);
regs(Word16);
regs(Word32);
regs(Word64);

#undef regs

#define assertRegsEmpty()                       \
        do {                                    \
                assert (0 == CPointerRegI);     \
                assert (0 == ObjptrRegI);       \
                assert (0 == Real32RegI);       \
                assert (0 == Real64RegI);       \
                assert (0 == Word8RegI);        \
                assert (0 == Word16RegI);       \
                assert (0 == Word32RegI);       \
                assert (0 == Word64RegI);       \
        } while (0)

typedef uintptr_t CodeOffset;

struct NameOffsets {
        CodeOffset codeOffset;  // An offset into code.
        Word32 nameOffset;  // An offset into addressNames.
};

typedef struct Bytecode {
        char *addressNames;
        Pointer code;
        CodeOffset codeSize;
        struct NameOffsets *nameOffsets;
        Word32 nameOffsetsSize;
} *Bytecode;


#define PopReg(ty) (assert (ty##RegI > 0), ty##Reg [--ty##RegI])
#define PopRegX(ty) PopReg(ty)
#define PushReg(ty) ty##Reg [ty##RegI++]
#define PushRegX(ty) PushReg(ty)

void MLton_callC (int i);  // provided by client
void MLton_Bytecode_interpret (Bytecode b, CodeOffset codeOffset);

#endif
