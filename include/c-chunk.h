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
                UNUSED ChunkFnPtr_t nextChunk;  \
                if (DEBUG_CCODEGEN)             \
                        fprintf (stderr, "%s:%d: %s(nextBlock = %d)\n", \
                                        __FILE__, __LINE__, #chunkName, (int)nextBlock); \
                goto doSwitchNextBlock;

#define EndDefineChunk                          \
        } /* end chunk */

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

/* ------------------------------------------------- */
/*  Operands                                         */
/* ------------------------------------------------- */

#define C(ty, x) (*(ty*)(x))
#define G(ty, i) (global##ty [i])
#define O(ty, b, o) (*(ty*)((b) + (o)))
#define S(ty, i) (*(ty*)(StackTop + (i)))
#define T(ty, i) T ## ty ## _ ## i
#define X(ty, b, i, s, o) (*(ty*)((b) + ((i) * (s)) + (o)))

#define GCState gcState
#define Frontier frontier
#define StackTop stackTop

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
