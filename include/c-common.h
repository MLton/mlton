/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */

#ifndef _C_COMMON_H_
#define _C_COMMON_H_

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

struct cont {
	void *nextChunk;
};

#define ChunkName(n) Chunk ## n

#define DeclareChunk(n)				\
	struct cont ChunkName(n)(void)

#define Chunkp(n) &(ChunkName(n))

#define PrepFarJump(n, l)				\
	do {						\
		cont.nextChunk = (void*)ChunkName(n);	\
		nextFun = l;				\
	} while (0)

#endif /* #ifndef _C_COMMON_H_ */
