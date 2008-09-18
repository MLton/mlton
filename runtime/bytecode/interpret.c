/* Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#ifndef MLTON_CODEGEN_STATIC_INLINE
#define MLTON_CODEGEN_STATIC_INLINE static inline
#endif

/* No need to declare inlined math functions, since <math.h> comes
 * with "platform.h".
 */
#ifndef MLTON_CODEGEN_MATHFN
#define MLTON_CODEGEN_MATHFN(decl)
#endif
/* WordS<N>_quot and WordS<N>_rem can be inlined with the
 * bytecode-codegen, since they will be used in a context where the
 * arguments are variables.
 */
#ifndef MLTON_CODEGEN_WORDSQUOTREM
#define MLTON_CODEGEN_WORDSQUOTREM(func) func
#endif
#ifndef MLTON_CODEGEN_WORDSQUOTREM_IMPL
#define MLTON_CODEGEN_WORDSQUOTREM_IMPL(func) func
#endif
/* No need to declare memcpy, since <string.h> comes with platform.h.
 */
#ifndef MLTON_CODEGEN_MEMCPY
#define MLTON_CODEGEN_MEMCPY(decl)
#endif
#include "platform.h"
#include "c-chunk.h"    // c-chunk.h must come before opcode.h because it
                        // redefines some opcode symbols

#include "interpret.h"
#include "opcode.h"

enum {
  DEBUG_BYTECODE = FALSE,
};

#if defined (GC_MODEL_NATIVE32)
#define WordPointer Word32
#define WordArrayIndex Word32
#elif defined (GC_MODEL_NATIVE64)
#define WordPointer Word64
#define WordArrayIndex Word64
#else
#error GC_MODEL_* undefined
#endif

typedef WordArrayIndex ArrayIndex;
typedef Word16 ArrayOffset;
typedef Word16 CallCIndex;
typedef Word16 GlobalIndex;
typedef uintptr_t Label;
typedef Int16 Offset;  // Offset must be signed.
typedef Pointer ProgramCounter;
typedef Word16 RegIndex;
typedef Word8 Scale;
typedef Int16 StackOffset;  // StackOffset must be signed.

PRIVATE extern struct GC_state gcState;

//----------------------------------------------------------------------
// Imports
//----------------------------------------------------------------------

#define regs(ty)                                \
        int ty##RegI = 0;                       \
        PRIVATE extern ty global##ty[];         \
        static ty ty##VReg[1000];               \
        ty ty##Reg[1000] = { 0 }

regs(CPointer);
regs(Objptr);
regs(Real32);
regs(Real64);
regs(Word8);
regs(Word16);
regs(Word32);
regs(Word64);

PRIVATE extern Objptr globalObjptrNonRoot[];

#undef regs

//
// Virtual Registers.  Explicitly referenced by the Machine IL.
//

#define R(ty, i) (ty##VReg [i])

//----------------------------------------------------------------------

#define Fetch(t, z)                                                             \
        do {                                                                    \
                z = *(t*)pc;                                                    \
                if (DEBUG or DEBUG_BYTECODE or disassemble) {                   \
                        if (! strcmp(#z,"label"))                               \
                                fprintf (stderr, " %s", offsetToLabel[z]);      \
                        else if (! strcmp(#z, "opc"))                           \
                                fprintf (stderr, " %d", (int)z);                \
                }                                                               \
                pc += sizeof (t);                                       \
        } while (0)

enum {
        MODE_load,
        MODE_store,
};

#define maybe unless (disassemble)

#define StoreReg(t, z) maybe PushReg(t) = z

#define loadStoreGen(mode, t, t2, z)            \
        switch (MODE_##mode) {                  \
        case MODE_load:                         \
                StoreReg (t2, (t2)z);           \
                break;                          \
        case MODE_store:                        \
                maybe z = (t) (PopReg (t2));    \
                break;                          \
        default:                                \
                assert (FALSE);                 \
        }

#define loadStore(mode, t, z)  loadStoreGen(mode, t, t, z)

#define loadStoreArrayOffset(mode, ty)                                          \
        case opcodeSymOfTy2 (ty, mode##ArrayOffset):                            \
        {                                                                       \
                ArrayOffset arrayOffset;                                        \
                Pointer arrayBase;                                              \
                ArrayIndex arrayIndex;                                          \
                Scale arrayScale;                                               \
                Fetch (ArrayOffset, arrayOffset);                               \
                Fetch (Scale, arrayScale);                                      \
                if (disassemble) goto mainLoop;                                 \
                arrayIndex = PopRegX (WordArrayIndex);                          \
                arrayBase = (Pointer) (PopRegX (WordPointer));                  \
                loadStore (mode, ty,                                            \
                                *(ty*)(arrayBase + (arrayIndex * arrayScale) + arrayOffset)); \
                goto mainLoop;                                                  \
        }

#define loadStoreContents(mode, ty)                             \
        case opcodeSymOfTy2 (ty, mode##Contents):               \
                if (disassemble) goto mainLoop;                 \
        {                                                       \
                Pointer base;                                   \
                base = (Pointer) (PopRegX (WordPointer));       \
                loadStore (mode, ty, C (ty, base));             \
                goto mainLoop;                                  \
        }

#define loadStoreFrontier(mode)                                 \
        case opcodeSym (mode##Frontier):                        \
                if (disassemble) goto mainLoop;                 \
                loadStoreGen (mode, Pointer, WordPointer, Frontier); \
                goto mainLoop;

#define loadGCState()                                   \
        case opcodeSym (loadGCState):                   \
                if (disassemble) goto mainLoop;         \
                StoreReg (WordPointer, (WordPointer)&gcState); \
                goto mainLoop;

#define loadStoreGlobal(mode, ty)                                       \
        case opcodeSymOfTy2 (ty, mode##Global):                         \
        {                                                               \
                GlobalIndex globalIndex;                                \
                Fetch (GlobalIndex, globalIndex);                       \
                if (disassemble) goto mainLoop;                         \
                loadStoreGen (mode, ty, ty, G (ty, globalIndex));       \
                goto mainLoop;                                          \
        }

#define loadStoreGlobalPointer(mode, ty)                                        \
        case opcodeSymOfTy2 (ty, mode##Global):                                 \
        {                                                                       \
                GlobalIndex globalIndex;                                        \
                Fetch (GlobalIndex, globalIndex);                               \
                if (disassemble) goto mainLoop;                                 \
                loadStoreGen (mode, ty, WordPointer, G (ty, globalIndex));      \
                goto mainLoop;                                                  \
        }

#define loadStoreGPNR(mode)                                                     \
        case opcodeSym (mode##GPNR):                                            \
        {                                                                       \
                GlobalIndex globalIndex;                                        \
                Fetch (GlobalIndex, globalIndex);                               \
                if (disassemble) goto mainLoop;                                 \
                loadStoreGen (mode, Objptr, WordPointer, GPNR (globalIndex));   \
                goto mainLoop;                                                  \
        }

#define loadStoreOffset(mode, ty)                                       \
        case opcodeSymOfTy2 (ty, mode##Offset):                         \
        {                                                               \
                Pointer base;                                           \
                Offset offset;                                          \
                Fetch (Offset, offset);                                 \
                if (disassemble) goto mainLoop;                         \
                base = (Pointer) (PopRegX (WordPointer));               \
                maybe loadStore (mode, ty, O (ty, base, offset));       \
                goto mainLoop;                                          \
        }

#define loadStoreRegister(mode, ty)                             \
        case opcodeSymOfTy2 (ty, mode##Register):               \
        {                                                       \
                RegIndex regIndex;                              \
                Fetch (RegIndex, regIndex);                     \
                if (disassemble) goto mainLoop;                 \
                loadStoreGen (mode, ty, ty, R (ty, regIndex));  \
                goto mainLoop;                                  \
        }

#define loadStoreRegisterPointer(mode, ty)                                      \
        case opcodeSymOfTy2 (ty, mode##Register):                               \
        {                                                                       \
                RegIndex regIndex;                                              \
                Fetch (RegIndex, regIndex);                                     \
                if (disassemble) goto mainLoop;                                 \
                loadStoreGen (mode, ty, WordPointer, R (ty, regIndex));         \
                goto mainLoop;                                                  \
        }

#define loadStoreStackOffset(mode, ty)                          \
        case opcodeSymOfTy2 (ty, mode##StackOffset):            \
        {                                                       \
                StackOffset stackOffset;                        \
                Fetch (StackOffset, stackOffset);               \
                if (disassemble) goto mainLoop;                 \
                loadStore (mode, ty, S (ty, stackOffset));      \
                goto mainLoop;                                  \
        }

#define loadStoreStackTop(mode)                                 \
        case opcodeSym (mode##StackTop):                        \
                if (disassemble) goto mainLoop;                 \
                loadStoreGen (mode, Pointer, WordPointer, StackTop); \
                goto mainLoop;

#define loadWord(size)                                  \
        case opcodeSymOfTy (Word, size, loadWord):      \
        {                                               \
                Word##size t0;                          \
                Fetch (Word##size, t0);                 \
                if (disassemble) goto mainLoop;         \
                loadStore (load, Word##size, t0);       \
                goto mainLoop;                          \
        }

#define binary(ty, f)                           \
        case opcodeSym (f):                     \
                if (disassemble) goto mainLoop; \
        {                                       \
                ty t0 = PopReg (ty);            \
                ty t1 = PopReg (ty);            \
                PushReg (ty) = f (t0, t1);      \
                goto mainLoop;                  \
        }

/* The bytecode interpreter relies on the fact that the overflow checking
 * primitives implemented in c-chunk.h only set the result if the operation does
 * not overflow.  When the result overflow, the interpreter pushes a zero on
 * the stack for the result.
 */
#define binaryCheck(ty, f)                                      \
        case opcodeSym (f):                                     \
                if (disassemble) goto mainLoop;                 \
        {                                                       \
                ty t0 = PopReg (ty);                            \
                ty t1 = PopReg (ty);                            \
                f (PushReg (ty), t0, t1, f##Overflow);          \
                overflow = FALSE;                               \
                goto mainLoop;                                  \
        f##Overflow:                                            \
                PushReg (ty) = 0; /* overflow, push 0 */        \
                overflow = TRUE;                                \
                goto mainLoop;                                  \
        }

#define coerceOp(n, f, t)  opcodeSym (f##_##n##To##t)
#define coerce(n, f1, t1, f2, t2)                       \
        case coerceOp (n, f2, t2):                      \
                if (disassemble) goto mainLoop;         \
        {                                               \
                f1 t0 = PopReg (f1);                    \
                PushReg (t1) = f2##_##n##To##t2 (t0);   \
                goto mainLoop;                          \
        }

#define compare(ty, f)                          \
        case opcodeSym (f):                     \
                if (disassemble) goto mainLoop; \
        {                                       \
                ty t0 = PopReg (ty);            \
                ty t1 = PopReg (ty);            \
                PushReg (Word32) = f (t0, t1);  \
                goto mainLoop;                  \
        }

#define cpointerBinary(f)                               \
        case opcodeSym (f):                             \
                if (disassemble) goto mainLoop;         \
        {                                               \
                Pointer t0;                             \
                t0 = (Pointer) (PopRegX (WordPointer)); \
                WordPointer t1 = PopRegX (WordPointer); \
                Pointer t2 = f (t0, t1);                \
                PushRegX (WordPointer) = (WordPointer) t2; \
                goto mainLoop;                          \
        }
#define cpointerCompare(f)                              \
        case opcodeSym (f):                             \
                if (disassemble) goto mainLoop;         \
        {                                               \
                Pointer t0, t1;                         \
                t0 = (Pointer) (PopRegX (WordPointer)); \
                t1 = (Pointer) (PopRegX (WordPointer)); \
                PushReg (Word32) = f (t0, t1);          \
                goto mainLoop;                          \
        }
#define cpointerCoerceFrom(f)                           \
        case opcodeSym (f):                             \
                if (disassemble) goto mainLoop;         \
        {                                               \
                WordPointer t0 = PopRegX (WordPointer); \
                Pointer t1 = f (t0);                    \
                PushRegX (WordPointer) = (WordPointer) t1; \
                goto mainLoop;                          \
        }
#define cpointerCoerceTo(f)                             \
        case opcodeSym (f):                             \
                if (disassemble) goto mainLoop;         \
        {                                               \
                Pointer t0;                             \
                t0 = (Pointer) (PopRegX (WordPointer)); \
                PushRegX (WordPointer) = f (t0);        \
                goto mainLoop;                          \
        }
#define cpointerDiff(f)                                 \
        case opcodeSym (f):                             \
                if (disassemble) goto mainLoop;         \
        {                                               \
                Pointer t0, t1;                         \
                t0 = (Pointer) (PopRegX (WordPointer)); \
                t1 = (Pointer) (PopRegX (WordPointer)); \
                PushRegX (WordPointer) = f (t0, t1);    \
                goto mainLoop;                          \
        }
#define cpointerLoadWord(f)                             \
        case opcodeSym (f):                             \
        {                                               \
                size_t t0;                              \
                Fetch (WordPointer, t0);                \
                if (disassemble) goto mainLoop;         \
                StoreReg (CPointer, (CPointer)t0);      \
                goto mainLoop;                          \
        }

#define shift(ty, f)                            \
        case opcodeSym (f):                     \
                if (disassemble) goto mainLoop; \
        {                                       \
                ty w = PopReg (ty);             \
                Word32 s = PopReg (Word32);     \
                ty w2 = f (w, s);               \
                PushReg (ty) = w2;              \
                goto mainLoop;                  \
        }

#define unary(ty, f)                            \
        case opcodeSym (f):                     \
                if (disassemble) goto mainLoop; \
        {                                       \
                ty t0 = PopReg (ty);            \
                PushReg (ty) = f (t0);          \
                goto mainLoop;                  \
        }

/* The bytecode interpreter relies on the fact that the overflow checking
 * primitives implemented in c-chunk.h only set the result if the operation does
 * not overflow.  When the result overflow, the interpreter pushes a zero on
 * the stack for the result.
 */
#define unaryCheck(ty, f)                                       \
        case opcodeSym (f):                                     \
                if (disassemble) goto mainLoop;                 \
        {                                                       \
                ty t0 = PopReg (ty);                            \
                f (PushReg (ty), t0, f##Overflow);              \
                overflow = FALSE;                               \
                goto mainLoop;                                  \
        f##Overflow:                                            \
                PushReg (ty) = 0; /* overflow, push 0 */        \
                overflow = TRUE;                                \
                goto mainLoop;                                  \
        }

#define Goto(l)                                 \
        do {                                    \
                maybe pc = code + l;            \
                goto mainLoop;                  \
        } while (0)

#define Switch(size)                                                    \
        case OPCODE_Switch##size:                                       \
        {                                                               \
                Label label;                                            \
                ProgramCounter lastCase;                                \
                Word##size test = 0;                                    \
                Word16 numCases;                                        \
                                                                        \
                Fetch (Word16, numCases);                               \
                if (sizeof(Label) == 4) {                               \
                        lastCase = pc + (4 + size/8) * numCases;        \
                } else if (sizeof(Label) == 8) {                        \
                        lastCase = pc + (8 + size/8) * numCases;        \
                } else { assert (FALSE); }                              \
                maybe test = PopReg (Word##size);                       \
                assertRegsEmpty ();                                     \
                while (pc < lastCase) {                                 \
                        Word##size caseWord;                            \
                        if (DEBUG or DEBUG_BYTECODE or disassemble)     \
                                fprintf (stderr, "\n\t  ");             \
                        Fetch (Word##size, caseWord);                   \
                        if (DEBUG or DEBUG_BYTECODE or disassemble)     \
                                fprintf (stderr, " =>");                \
                        Fetch (Label, label);                           \
                        if (not disassemble and test == caseWord)       \
                                Goto (label);                           \
                }                                                       \
                goto mainLoop;                                          \
        }

typedef char *String;

#undef CacheFrontier
#undef CacheStackTop
#undef FlushFrontier
#undef FlushStackTop
#define CacheFrontier()                         \
        do {                                    \
                frontier = gcState.frontier;    \
        } while (0)
#define CacheStackTop()                         \
        do {                                    \
                stackTop = gcState.stackTop;    \
        } while (0)
#define FlushFrontier()                         \
        do {                                    \
                gcState.frontier = frontier;    \
        } while (0)
#define FlushStackTop()                         \
        do {                                    \
                gcState.stackTop = stackTop;    \
        } while (0)

#define disp(ty,ty2,fmt)                                        \
        for (i = 0; i < ty##RegI; ++i)                          \
                fprintf (stderr, "\n" #ty "Reg[%d] = "fmt,      \
                                i, (ty2)(ty##Reg[i]))

static inline void displayRegs (void) {
        int i;

        disp (CPointer,uintptr_t,FMTPTR);
        disp (Objptr,uintptr_t,FMTPTR);
        disp (Word8,Word8,"0x%02"PRIx8);
        disp (Word16,Word16,"0x%04"PRIx16);
        disp (Word32,Word32,"0x%08"PRIx32);
        disp (Word64,Word64,"0x%016"PRIx64);
        disp (Real32,Real32,"%f");
        disp (Real64,Real64,"%f");
}

static void interpret (Bytecode b, CodeOffset codeOffset, Bool disassemble) {
        CallCIndex callCIndex;
        Pointer code;
        Pointer frontier;
        unsigned int i;
        String name;
        String *offsetToLabel = NULL;
        Opcode opc;
        Bool overflow = FALSE;
        ProgramCounter pc;
        ProgramCounter pcMax;
        Pointer stackTop;

        code = b->code;
        pcMax = b->code + b->codeSize;
        if (DEBUG or DEBUG_BYTECODE or disassemble) {
                offsetToLabel =
                  (String*)(calloc_safe (b->codeSize, sizeof(*offsetToLabel)));
                for (i = 0; i < b->nameOffsetsSize; ++i)
                        offsetToLabel [b->nameOffsets[i].codeOffset] =
                                b->addressNames + b->nameOffsets[i].nameOffset;
        }
        if (disassemble)
                pc = code;
        else {
                pc = code + codeOffset;
        }
        CacheFrontier ();
        CacheStackTop ();
mainLoop:
        if (DEBUG_BYTECODE)
                displayRegs ();
        if (DEBUG or DEBUG_BYTECODE or disassemble) {
                if (pc == pcMax)
                        goto done;
                name = offsetToLabel [pc - b->code];
                unless (NULL == name)
                        fprintf (stderr, "\n%s:", name);
                fprintf (stderr, "\n\t");
        }
        assert (code <= pc and pc < pcMax);
        Fetch (Opcode, opc);
        assert (opc < (cardof (opcodeStrings)));
        if (DEBUG or DEBUG_BYTECODE or disassemble)
                fprintf (stderr, "%s", opcodeStrings[opc]);
        switch ((enum OpcodeEnum)opc) {
        prims ();
        case opcodeSym (CacheFrontier):
        {
                if (disassemble) goto mainLoop;
                CacheFrontier ();
                goto mainLoop;
        }
        case opcodeSym (FlushFrontier):
        {
                if (disassemble) goto mainLoop;
                FlushFrontier ();
                goto mainLoop;
        }
        case opcodeSym (CacheStackTop):
        {
                if (disassemble) goto mainLoop;
                CacheStackTop ();
                goto mainLoop;
        }
        case opcodeSym (FlushStackTop):
        {
                if (disassemble) goto mainLoop;
                FlushStackTop ();
                goto mainLoop;
        }
        case opcodeSym (BranchIfZero):
        {
                Label label;

                Fetch (Label, label);
                if (disassemble) goto mainLoop;
                if (0 == PopReg (Word32))
                        Goto (label);
                goto mainLoop;
        }
        case opcodeSym (CallC):
                Fetch (CallCIndex, callCIndex);
                unless (disassemble) {
                        FlushFrontier ();
                        FlushStackTop ();
                        MLton_callC (callCIndex);
                        CacheFrontier ();
                        CacheStackTop ();
                }
                goto mainLoop;
        case opcodeSym (Goto):
        {
                Label label;
                Fetch (Label, label);
                Goto (label);
        }
        loadStoreGPNR(load);
        loadStoreGPNR(store);
        case opcodeSym (JumpOnOverflow):
        {
                Label label;
                Fetch (Label, label);
                if (overflow)
                        Goto (label);
                goto mainLoop;
        }
        case opcodeSym (Raise):
                maybe StackTop = gcState.stackBottom + gcState.exnStack;
                // fall through to Return.
        case opcodeSym (Return):
                Goto (*(Label*)(StackTop - sizeof (Label)));
        Switch(8);
        Switch(16);
        Switch(32);
        Switch(64);
        case opcodeSym (Thread_returnToC):
                if (disassemble) goto mainLoop;
                FlushFrontier ();
                FlushStackTop ();
                goto done;
        default:
                assert (FALSE);
        }
        assert (FALSE);
done:
        if (DEBUG or DEBUG_BYTECODE or disassemble)
                free (offsetToLabel);
        return;
}

static void disassemble (Bytecode b, CodeOffset codeOffset) {
        interpret (b, codeOffset, TRUE);
        fprintf (stderr, "\n");
}

void MLton_Bytecode_interpret (Bytecode b, CodeOffset codeOffset) {
        if (DEBUG or DEBUG_BYTECODE) {
                fprintf (stderr, "MLton_Bytecode_interpret ("FMTPTR", %"PRIuPTR")\n",
                                (uintptr_t)b,
                                codeOffset);
                disassemble (b, codeOffset);
                fprintf (stderr, "interpret starting\n");
        }
        interpret (b, codeOffset, FALSE);
}
