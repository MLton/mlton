/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_COMMON_H_
#define _C_COMMON_H_

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

#include "export.h"

typedef uintptr_t ChunkFn_t (CPointer, CPointer, CPointer, uintptr_t);
typedef ChunkFn_t *ChunkFnPtr_t;

#define ChunkName(n) Chunk ## n

#define DeclareChunk(n) PRIVATE extern ChunkFn_t ChunkName(n);

#define Chunkp(n) &(ChunkName(n))

#endif /* #ifndef _C_COMMON_H_ */
