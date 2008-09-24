/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _OPCODE_H_
#define _OPCODE_H_

#define coercePrims()                           \
        allWordCoercePrims(8)                   \
        allWordCoercePrims(16)                  \
        allWordCoercePrims(32)                  \
        allWordCoercePrims(64)                  \
        coerce(rnd, Real32, Real32, Real32, Real32)     \
        coerce(rnd, Real32, Real64, Real32, Real64)     \
        coerce(rnd, Real64, Real32, Real64, Real32)     \
        coerce(rnd, Real64, Real64, Real64, Real64)     \
        coerce(cast, Real32, Word32, Real32, Word32)    \
        coerce(cast, Word32, Real32, Word32, Real32)    \
        coerce(cast, Real64, Word64, Real64, Word64)    \
        coerce(cast, Word64, Real64, Word64, Real64)

#define allWordCoercePrims(size)                        \
        bothFromWordCoercePrims(rnd, size, Real32)      \
        bothFromWordCoercePrims(rnd, size, Real64)      \
        bothToWordCoercePrims(rnd, Real32, size)        \
        bothToWordCoercePrims(rnd, Real64, size)        \
        bothFromWordCoercePrims(extd, size, Word8)      \
        bothFromWordCoercePrims(extd, size, Word16)     \
        bothFromWordCoercePrims(extd, size, Word32)     \
        bothFromWordCoercePrims(extd, size, Word64)

#define bothFromWordCoercePrims(name, from, to)                 \
        coerce (name, Word##from, to, Word##S##from, to)        \
        coerce (name, Word##from, to, Word##U##from, to)
#define bothToWordCoercePrims(name, from, to)                   \
        coerce (name, from, Word##to, from, Word##S##to)        \
        coerce (name, from, Word##to, from, Word##U##to)

#define loadStorePrimsOfTy(mode, ty)            \
        loadStoreArrayOffset (mode, ty)         \
        loadStoreContents (mode, ty)            \
        loadStoreGlobal (mode, ty)              \
        loadStoreOffset (mode, ty)              \
        loadStoreRegister (mode, ty)            \
        loadStoreStackOffset (mode, ty)

#define loadStorePrims(mode)                            \
        loadStorePrimsOfTy (mode, Real32)               \
        loadStorePrimsOfTy (mode, Real64)               \
        loadStorePrimsOfTy (mode, Word8)                \
        loadStorePrimsOfTy (mode, Word16)               \
        loadStorePrimsOfTy (mode, Word32)               \
        loadStorePrimsOfTy (mode, Word64)               \
        loadStoreGlobalPointer (mode, CPointer)         \
        loadStoreGlobalPointer (mode, Objptr)           \
        loadStoreRegisterPointer (mode, CPointer)       \
        loadStoreRegisterPointer (mode, Objptr)         \
        loadStoreFrontier (mode)                        \
        loadStoreStackTop (mode)

#define realPrimsOfSize(size)                           \
        binary (Real##size, Real##size##_add)           \
        binary (Real##size, Real##size##_div)           \
        compare (Real##size, Real##size##_equal)        \
        compare (Real##size, Real##size##_le)           \
        compare (Real##size, Real##size##_lt)           \
        binary (Real##size, Real##size##_mul)           \
        unary (Real##size, Real##size##_neg)            \
        unary (Real##size, Real##size##_round)          \
        binary (Real##size, Real##size##_sub)           \
        unary (Real##size, Real##size##_Math_acos)      \
        unary (Real##size, Real##size##_Math_asin)      \
        unary (Real##size, Real##size##_Math_atan)      \
        binary (Real##size, Real##size##_Math_atan2)    \
        unary (Real##size, Real##size##_Math_cos)       \
        unary (Real##size, Real##size##_Math_exp)       \
        unary (Real##size, Real##size##_Math_ln)        \
        unary (Real##size, Real##size##_Math_log10)     \
        unary (Real##size, Real##size##_Math_sin)       \
        unary (Real##size, Real##size##_Math_sqrt)      \
        unary (Real##size, Real##size##_Math_tan)

#define wordPrimsOfSize(size)                           \
        binary (Word##size, Word##size##_add)           \
        binary (Word##size, Word##size##_andb)          \
        compare (Word##size, Word##size##_equal)        \
        compare (Word##size, WordS##size##_lt)          \
        compare (Word##size, WordU##size##_lt)          \
        shift (Word##size, Word##size##_lshift)         \
        binary (Word##size, WordS##size##_mul)          \
        binary (Word##size, WordU##size##_mul)          \
        unary (Word##size, Word##size##_neg)            \
        unary (Word##size, Word##size##_notb)           \
        binary (Word##size, Word##size##_orb)           \
        binary (Word##size, WordS##size##_quot)         \
        binary (Word##size, WordU##size##_quot)         \
        binary (Word##size, WordS##size##_rem)          \
        binary (Word##size, WordU##size##_rem)          \
        shift (Word##size, Word##size##_rol)            \
        shift (Word##size, Word##size##_ror)            \
        shift (Word##size, WordS##size##_rshift)        \
        shift (Word##size, WordU##size##_rshift)        \
        binary (Word##size, Word##size##_sub)           \
        binary (Word##size, Word##size##_xorb)          \
        binaryCheck (Word##size, WordS##size##_addCheck)        \
        binaryCheck (Word##size, WordU##size##_addCheck)        \
        binaryCheck (Word##size, WordS##size##_mulCheck)        \
        binaryCheck (Word##size, WordU##size##_mulCheck)        \
        unaryCheck (Word##size, Word##size##_negCheck)          \
        binaryCheck (Word##size, WordS##size##_subCheck)        \
        loadWord (size)

#define cpointerPrims()                                 \
        cpointerBinary (CPointer_add)                   \
        cpointerBinary (CPointer_sub)                   \
        cpointerCompare(CPointer_equal)                 \
        cpointerCompare(CPointer_lt)                    \
        cpointerCoerceFrom (CPointer_fromWord)          \
        cpointerCoerceTo (CPointer_toWord)              \
        cpointerDiff (CPointer_diff)                    \
        cpointerLoadWord (CPointer_loadWord)

#define prims()                                         \
        coercePrims ()                                  \
        cpointerPrims ()                                \
        loadGCState ()                                  \
        loadStorePrims (load)                           \
        loadStorePrims (store)                          \
        realPrimsOfSize (32)                            \
        realPrimsOfSize (64)                            \
        wordPrimsOfSize (8)                             \
        wordPrimsOfSize (16)                            \
        wordPrimsOfSize (32)                            \
        wordPrimsOfSize (64)

#define opcodes()                               \
        prims()                                 \
        opcodeGen (CacheFrontier)               \
        opcodeGen (FlushFrontier)               \
        opcodeGen (CacheStackTop)               \
        opcodeGen (FlushStackTop)               \
        opcodeGen (BranchIfZero)                \
        opcodeGen (CallC)                       \
        opcodeGen (Goto)                        \
        opcodeGen (loadGPNR)                    \
        opcodeGen (storeGPNR)                   \
        opcodeGen (JumpOnOverflow)              \
        opcodeGen (Raise)                       \
        opcodeGen (Return)                      \
        opcodeGen (Switch8)                     \
        opcodeGen (Switch16)                    \
        opcodeGen (Switch32)                    \
        opcodeGen (Switch64)                    \
        opcodeGen (Thread_returnToC)

#define opcodeSym(z)  OPCODE_##z
#define opcodeSymOfTy(ty, size, name)  opcodeSym (ty##size##_##name)
#define opcodeSymOfTy2(ty, name)  opcodeSym (ty##_##name)
#define opcodeName(ty, size, name) opcodeGen (ty##size##_##name)
#define opcodeName2(ty, name) opcodeGen (ty##_##name)

#define binary(ty, f)  opcodeGen (f)
#define binaryCheck(ty, f)  opcodeGen (f)
#define coerceOp(n, f, t)  opcodeGen (f##_##n##To##t)
#define coerce(n, f1, t1, f2, t2)  coerceOp (n, f2, t2)
#define compare(ty, f)  opcodeGen (f)
#define cpointerBinary(f)  opcodeGen (f)
#define cpointerCompare(f)  opcodeGen (f)
#define cpointerCoerceFrom(f)  opcodeGen (f)
#define cpointerCoerceTo(f)  opcodeGen (f)
#define cpointerDiff(f)  opcodeGen (f)
#define cpointerLoadWord(f)  opcodeGen (f)
#define loadStoreArrayOffset(mode, ty)  opcodeName2 (ty, mode##ArrayOffset)
#define loadStoreContents(mode, ty)  opcodeName2 (ty, mode##Contents)
#define loadStoreFrontier(mode) opcodeGen (mode##Frontier)
#define loadGCState() opcodeGen (loadGCState)
#define loadStoreGlobal(mode, ty)  opcodeName2 (ty, mode##Global)
#define loadStoreGlobalPointer(mode, ty)  opcodeName2 (ty, mode##Global)
#define loadStoreOffset(mode, ty)  opcodeName2 (ty, mode##Offset)
#define loadStoreRegister(mode, ty)  opcodeName2 (ty, mode##Register)
#define loadStoreRegisterPointer(mode, ty)  opcodeName2 (ty, mode##Register)
#define loadStoreStackOffset(mode, ty)  opcodeName2 (ty, mode##StackOffset)
#define loadStoreStackTop(mode)  opcodeGen (mode##StackTop)
#define loadWord(size)  opcodeName (Word, size, loadWord)
#define shift(ty, f)  opcodeGen (f)
#define unary(ty, f)  opcodeGen (f)
#define unaryCheck(ty, f)  opcodeGen (f)

// Define the opcode strings.

#define opcodeGen(z)  #z,

PRIVATE const char *opcodeStrings [] = {
        opcodes ()
};

#undef opcodeGen

// Define the Opcode enum.

#define opcodeGen(z) opcodeSym (z),

enum OpcodeEnum {
        opcodes ()
};

typedef Word16 Opcode;

#undef binary
#undef binaryCheck
#undef coerce
#undef coerceOp
#undef compare
#undef cpointerBinary
#undef cpointerCompare
#undef cpointerCoerceFrom
#undef cpointerCoerceTo
#undef cpointerDiff
#undef cpointerLoadWord
#undef loadGCState
#undef loadStoreArrayOffset
#undef loadStoreContents
#undef loadStoreFrontier
#undef loadStoreGlobal
#undef loadStoreGlobalPointer
#undef loadStoreOffset
#undef loadStoreRegister
#undef loadStoreRegisterPointer
#undef loadStoreStackOffset
#undef loadStoreStackTop
#undef loadWord
#undef shift
#undef unary
#undef unaryCheck

// At this point the opcodes() macro is still defined.

#endif
