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
