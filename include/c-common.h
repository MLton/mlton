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

#define ChunkName(n) Chunk ## n

#define DeclareChunk(n)                         \
        PRIVATE uintptr_t ChunkName(n)(uintptr_t nextBlock, Pointer stackTop, Pointer frontier)

#define Chunkp(n) &(ChunkName(n))

#endif /* #ifndef _C_COMMON_H_ */
