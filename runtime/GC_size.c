#include "gc.h"

/* ------------------------------------------------- */
/*                Depth first search                 */
/* ------------------------------------------------- */

static inline bool isMarked(pointer p) {
	return (GC_getHeader(p) & MARK_BIT);
}

static inline void mark(pointer p) {
	*(GC_getHeaderp(p)) = GC_getHeader(p) | MARK_BIT;
}

static inline void unmark(pointer p) {
	*(GC_getHeaderp(p)) = GC_getHeader(p) & ~MARK_BIT;
}

static inline bool dfsIsEmpty(GC_state s) {
	return (s->dfsTop == s->dfsBottom);
}

static inline pointer dfsPop(GC_state s) {
	return (*--(pointer*)s->dfsTop);
}

static inline void dfsPushIfMarked(GC_state s, pointer *pp) {
	pointer p;

	p = *pp;
	if (isMarked(p)) {
		unmark(p);
		*((pointer*)s->dfsTop)++ = p;
	}
}

static inline void dfsPushIfUnmarked(GC_state s, pointer *pp) {
	pointer p;

	p = *pp;
	if (not isMarked(p)) {
		mark(p);
		*((pointer*)s->dfsTop)++ = p;
	}
}

/*
 * Set up the stack in to space.  Make sure to space is as big as from space
 * so that we are guaranteed the stack won't overflow.
 */
static void dfsInitializeStack(GC_state s) {
	if (s->toSize < s->fromSize and s->toBase != NULL) {
		smunmap(s->toBase, s->toSize);
		s->toBase = NULL;
	}
	if (s->toBase == NULL) {
		s->toSize = s->fromSize;
		GC_toSpace(s);
	}
	assert(s->toBase != NULL);
	assert(s->toSize >= s->fromSize);
	s->dfsBottom = s->toBase;
	s->dfsTop = s->dfsBottom;
}

/* ------------------------------------------------- */
/*                      GC_size                      */
/* ------------------------------------------------- */
/* 
 * Descend the object once, marking it and computing the size.
 * Descend the object again, clearing the mark bits.
 */

#define DISPLAY_SIZES FALSE
#if DISPLAY_SIZES
struct Size {
	uint bytes;
	uint count;
};
enum {
	NUM_SIZES = 10000,
};
static struct Size sizes [NUM_SIZES];
#endif

static void dfsSize(GC_state s, pointer root, uint *size, uint *markStackSize) {
	uint sz, total;
	pointer maxTop;

#if DISPLAY_SIZES
	{
		int i;
		
		for (i = 0; i < NUM_SIZES; ++i) {
			sizes[i].bytes = 0;
		}
	}
#endif
	dfsInitializeStack(s);
	total = 0;
	maxTop = s->dfsTop;
	dfsPushIfUnmarked(s, &root);
	while (not(dfsIsEmpty(s))) {
		pointer p;

		if (s->dfsTop > maxTop) 
			maxTop = s->dfsTop;
		p = dfsPop(s);
		sz = GC_objectSize(p);
		total += sz;
#if DISPLAY_SIZES
		{
			int i;
			uint header;

			header = GC_getHeader(p);
			for (i = 0; TRUE; ++i) {
				if (0 == sizes[i].bytes) {
					sizes[i].bytes = header;
					sizes[i].count = 1;
					break;
				} else if (header == sizes[i].bytes) {
					sizes[i].count++;
					break;
				}
			}
		}
#endif
		GC_foreachPointerInObject(s, dfsPushIfUnmarked, p);
	}
	*size = total;
#if DISPLAY_SIZES
		{
			int i;
	
			for (i = 0; 0 != sizes[i].bytes; ++i) {
				fprintf(stderr, "header = %x  count = %d\n",
					sizes[i].bytes, sizes[i].count);
			}
		}
#endif
	*markStackSize = maxTop - s->dfsBottom;
}

static void unmarkDeep(GC_state s, pointer root) {
	/* DFS, unmark the object. */
	dfsPushIfMarked(s, &root);
	while (not(dfsIsEmpty(s)))
		GC_foreachPointerInObject(s, dfsPushIfMarked, dfsPop(s));
}

uint GC_size(GC_state s, pointer root) {
	uint size;
	uint dfsStackSize;

/*	fprintf (stderr, "GC_size starting\n"); */
	/* If it's not an object, it has no heap space. */
	if (not GC_isPointer(root))
		return 0;
	dfsSize(s, root, &size, &dfsStackSize);
	unmarkDeep(s, root);
/*	fprintf (stderr, "GC_size finishing %d\n", size); */
	return size;
}
