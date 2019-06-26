/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_CHUNK_H_
#define _C_CHUNK_H_

#include <stdio.h>
/* `memcpy` is used by coercion `<ty>_castTo<ty>` functions (`basis/coerce.h`)
 * and by misaligned `<ty>_fetch`, `<ty>_store`, and `<ty>_move` functions
 * (`basis/Real/Real-ops.h` and `basis/Word/Word-ops.h`)
 */
#include <string.h>
/* Math functions used by `Real<n>_f` functions (`basis/Real/Real-ops.h`).
 */
#include <math.h>

#include "ml-types.h"
#include "c-types.h"
#include "c-common.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

#define UNUSED __attribute__ ((unused))
#define NORETURN __attribute__ ((noreturn))
#define Unreachable() __builtin_unreachable()

/* ------------------------------------------------- */
/* Chunk                                             */
/* ------------------------------------------------- */

#define Chunk(n)                                \
        PRIVATE uintptr_t ChunkName(n)(CPointer gcState, CPointer stackTop, CPointer frontier, uintptr_t nextBlock) { \
                const ChunkFnPtr_t selfChunk = Chunkp(n);                \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: Chunk%d(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, n, (int)nextBlock); \
                SwitchNextBlock();

#define EndChunk                                \
        } /* end chunk */

#define LeaveChunk(nextChunk, nextBlock)        \
        do {                                    \
                /* interchunk return */         \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: LeaveChunk(nextChunk = \"%s\", nextBlock = %d)\n", \
                                        __FILE__, __LINE__, #nextChunk, (int)nextBlock); \
                if (TailCall) {                 \
                        return nextChunk(gcState, stackTop, frontier, nextBlock); \
                } else {                        \
                        FlushFrontier();        \
                        FlushStackTop();        \
                        return nextBlock;       \
                }                               \
        } while (0)

/* ------------------------------------------------- */
/*  ChunkSwitch                                      */
/* ------------------------------------------------- */

#define ChunkSwitch                             \
                doSwitchNextBlock:              \
                switch (nextBlock) {

#define ChunkSwitchCase(index, label)           \
                case index: goto label;

#define EndChunkSwitch                          \
                default:                        \
                        Unreachable();          \
                } /* end switch (nextBlock) */

#define SwitchNextBlock()                       \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: SwitchNextBlock(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, (int)nextBlock); \
                goto doSwitchNextBlock

/* ------------------------------------------------- */
/*  Operands                                         */
/* ------------------------------------------------- */

#define C(ty, x) (*(ty*)(x))
#define G(ty, i) (global##ty [i])
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define X(ty, b, i, s, o) (*(ty*)((b) + ((i) * (s)) + (o)))
#define S(ty, i) (*(ty*)(StackTop + (i)))

#define GCState gcState
#define Frontier frontier
#define StackTop stackTop

#define ExnStack *(size_t*)(GCState + ExnStackOffset)
#define FrontierMem *(Pointer*)(GCState + FrontierOffset)
#define StackBottom *(Pointer*)(GCState + StackBottomOffset)
#define StackTopMem *(Pointer*)(GCState + StackTopOffset)

/* ------------------------------------------------- */
/* Cache and Flush                                   */
/* ------------------------------------------------- */

#define CacheFrontier()                         \
        do {                                    \
                Frontier = FrontierMem;         \
        } while (0)

#define CacheStackTop()                         \
        do {                                    \
                StackTop = StackTopMem;         \
        } while (0)

#define FlushFrontier()                         \
        do {                                    \
                FrontierMem = Frontier;         \
        } while (0)

#define FlushStackTop()                         \
        do {                                    \
                StackTopMem = StackTop;         \
        } while (0)

/* ------------------------------------------------- */
/* Stack                                             */
/* ------------------------------------------------- */

#define Push(bytes)                                                     \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Push (%d)\n",          \
                                        __FILE__, __LINE__, bytes);     \
                StackTop += (bytes);                                    \
        } while (0)

/* ------------------------------------------------- */
/* Transfers                                         */
/* ------------------------------------------------- */

#define BNZ(x, lnz, lz)                                                 \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: BNZ(%llu, %s, %s)\n",  \
                                        __FILE__, __LINE__, ((unsigned long long)x), #lnz, #lz); \
                if (x) goto lnz; else goto lz;                          \
        } while (0)

#define NearCall(l)                             \
        goto l

#define FarCall(nextChunk, nextBlock)           \
        do {                                    \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: FarCall(%d, %d)\n", \
                                        __FILE__, __LINE__, (int)nextChunk, (int)nextBlock); \
                LeaveChunk(ChunkName(nextChunk), nextBlock); \
        } while (0)

#define Return(mustReturnToSelf,mayReturnToSelf,mustReturnToOther)              \
        do {                                                                    \
                nextBlock = *(uintptr_t*)(StackTop - sizeof(uintptr_t));        \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Return()  nextBlock = %d\n",   \
                                        __FILE__, __LINE__, (int)nextBlock);    \
                ChunkFnPtr_t nextChunk = nextChunks[nextBlock];                 \
                if (mustReturnToSelf                                            \
                    || (mayReturnToSelf && (nextChunk == selfChunk))) {         \
                        SwitchNextBlock();                                      \
                } else if (mustReturnToOther) {                                 \
                        LeaveChunk((*mustReturnToOther), nextBlock);            \
                } else {                                                        \
                        LeaveChunk((*nextChunk), nextBlock);                    \
                }                                                               \
        } while (0)

#define Raise(mustRaiseToSelf,mayRaiseToSelf,mustRaiseToOther)                  \
        do {                                                                    \
                if (DEBUG_CCODEGEN)                                             \
                        fprintf (stderr, "%s:%d: Raise()\n",                    \
                                        __FILE__, __LINE__);                    \
                StackTop = StackBottom + ExnStack;                              \
                Return(mustRaiseToSelf,mayRaiseToSelf,mustRaiseToOther);        \
        } while (0)


/* ------------------------------------------------- */
/* Calling SML from C                                */
/* ------------------------------------------------- */

#define Thread_returnToC()                                              \
        do {                                                            \
                if (DEBUG_CCODEGEN)                                     \
                        fprintf (stderr, "%s:%d: Thread_returnToC()\n", \
                                        __FILE__, __LINE__);            \
                return (uintptr_t)-1;                                   \
        } while (0)

/* ------------------------------------------------- */
/* Primitives                                        */
/* ------------------------------------------------- */

#ifndef MLTON_CODEGEN_STATIC_INLINE
#define MLTON_CODEGEN_STATIC_INLINE static inline
#endif
#include "basis/coerce.h"
#include "basis/cpointer.h"
#include "basis/Real/Real-ops.h"
#include "basis/Word/Word-ops.h"

#endif /* #ifndef _C_CHUNK_H_ */
