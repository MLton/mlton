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

#define Expect(x,c) __builtin_expect(x, c)
#define UNUSED __attribute__ ((unused))
#define Unreachable() __builtin_unreachable()

/* ------------------------------------------------- */
/* Chunk                                             */
/* ------------------------------------------------- */

#define DefineChunk(chunkName)                  \
        PRIVATE uintptr_t chunkName(UNUSED CPointer gcState, UNUSED CPointer stackTop, UNUSED CPointer frontier, uintptr_t nextBlock) { \
                UNUSED static const ChunkFnPtr_t selfChunk = &(chunkName); \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: %s(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, #chunkName, (int)nextBlock); \
                SwitchNextBlock();

#define EndDefineChunk                          \
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

#if JumpTable

#define ChunkSwitch(firstIndex, length)         \
                static const uintptr_t nextLabelsBias = firstIndex; \
                static const void* nextLabels[length] = {

#define ChunkSwitchCase(index, label)           \
                &&label,

#define EndChunkSwitch                          \
                };                              \
                doSwitchNextBlock:              \
                goto *nextLabels[nextBlock - nextLabelsBias];

#else

#define ChunkSwitch(firstIndex, length)         \
                doSwitchNextBlock:              \
                switch (nextBlock) {

#define ChunkSwitchCase(index, label)           \
                case index: goto label;

#define EndChunkSwitch                          \
                default:                        \
                        Unreachable();          \
                } /* end switch (nextBlock) */

#endif

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

#define FrontierMem *(Pointer*)(GCState + FrontierOffset)
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
                        fprintf (stderr, "%s:%d: FarCall(%s, %d)\n", \
                                        __FILE__, __LINE__, #nextChunk, (int)nextBlock); \
                LeaveChunk(nextChunk, nextBlock); \
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
                } else if ((void*)mustReturnToOther != NULL) {                  \
                        LeaveChunk((*mustReturnToOther), nextBlock);            \
                } else {                                                        \
                        LeaveChunk((*nextChunk), nextBlock);                    \
                }                                                               \
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
