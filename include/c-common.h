/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

struct cont {
        void *nextChunk;
};

PRIVATE extern uintptr_t nextFun;
PRIVATE extern int returnToC;
PRIVATE extern struct cont (*nextChunks []) (void);

#define ChunkName(n) Chunk ## n

#define DeclareChunk(n)                         \
        PRIVATE struct cont ChunkName(n)(void)

#define Chunkp(n) &(ChunkName(n))

#define PrepFarJump(n, l)                               \
        do {                                            \
                cont.nextChunk = (void*)ChunkName(n);   \
                nextFun = l;                            \
        } while (0)

#endif /* #ifndef _C_COMMON_H_ */
