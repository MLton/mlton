/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */
#ifndef _MLTON_GC_H
#define _MLTON_GC_H
/*
 * A stop-and-copy GC with a fixed size heap.
 * Handles fixed size objects, arrays, and continutations.
 */

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>

#include "my-lib.h"

typedef uint word;
typedef char* pointer;

/* Sizes are (almost) always measured in bytes. */
enum {
	WORD_SIZE = 4,
	GC_OBJECT_HEADER_SIZE = WORD_SIZE,
	GC_ARRAY_HEADER_SIZE = WORD_SIZE + GC_OBJECT_HEADER_SIZE,
/*
 * Array lengths can't have the top two bits set because the high bit is used
 * to distinguish between objects and arrays, and the next bit is the mark bit.
 * They also can't be 30 one bits, because that is reserved for the stack object
 * header, which must be distinguishable from an array length.
 */
	GC_MAX_ARRAY_LENGTH = 0x3FFFFFFF,
	/* High bit in an object identifies its type.
	 * Normal objects have a high bit of 1.
	 * Arrays and stacks have a high bit of 0.
	 */
	HIGH_BIT = 0x80000000,
	/* The mark bit in an object is used by runtime utilities that need to 
	 * perform a depth first search of objects.
	 */
	MARK_BIT = 0x40000000,
	/* Number of bits specifying the number of nonpointers in an object. */
	NON_POINTER_BITS = 15,
	/* Number of bits specifying the number of pointers in an object. */
	POINTER_BITS = 15,
	NON_POINTERS_SHIFT = POINTER_BITS,
	POINTER_SIZE = WORD_SIZE,
};

#define TWOPOWER(n) (1 << (n))

/* ONES(n) is a word with n one bits at the bottom. */
#define ONES(n) (TWOPOWER(n) - 1)

/*
 * Build the one word header for an object, given the number of words of
 * nonpointers and the number of pointers.
 */
static inline word GC_objectHeader(uint np, uint p) {
	assert(np < TWOPOWER(NON_POINTER_BITS));
	assert(p < TWOPOWER(POINTER_BITS));
	return HIGH_BIT | p | (np << NON_POINTERS_SHIFT);
}

/*
 * Build the one word header for an array, given the number of bytes of
 * nonpointers and the number of pointers.
 */
static inline word GC_arrayHeader(uint np, uint p) {
	/* Arrays are allowed one fewer non pointer bit, because the top 
  	 * non pointer bit is used for the continuation header word.
         */
	assert(np < TWOPOWER(NON_POINTER_BITS - 1));
	assert(p < TWOPOWER(POINTER_BITS));
	return p | (np << NON_POINTERS_SHIFT);
}

/* ------------------------------------------------- */
/*                   GC_isPointer                    */
/* ------------------------------------------------- */

/* Returns true if p looks like a pointer, i.e. if p = 0 mod 4. */
static inline bool GC_isPointer(pointer p) {
	return(0 == ((word)p & 0x3));
}

static inline uint wordAlign(uint p) {
 	return ((p + 3) & ~ 3);
}

/* ------------------------------------------------- */
/*                  GC_frameLayout                   */
/* ------------------------------------------------- */

typedef ushort *GC_offsets;

typedef struct GC_frameLayout {
	/* Number of bytes in frame, including space for return address. */
	ushort numBytes;
	/* Offsets from stackTop pointing at bottom of frame at which pointers
	 * are located. 
	 */
	GC_offsets offsets;
} GC_frameLayout;

/* ------------------------------------------------- */
/*                     GC_stack                      */
/* ------------------------------------------------- */

typedef struct GC_stack {
	uint reserved;	/* Number of bytes reserved for stack. */
	uint used;	/* Number of bytes in use. */
	/* The next address is the bottom of the stack, and the following
         * reserved bytes hold space for the stack.
         */
} *GC_stack;

/* ------------------------------------------------- */
/*                     GC_thread                     */
/* ------------------------------------------------- */

typedef struct GC_thread {
	/* The order of these two fields is important.  The nonpointer field
	 * must be first, because this object must appear to be a normal heap
	 * object.
	 */
	uint exnStack;    	/* An offset added to stackBottom that specifies 
				 * where the top of the exnStack is.
				 */
	GC_stack stack;		/* The stack for this thread. */
} *GC_thread;

/* ------------------------------------------------- */
/*                     GC_state                      */
/* ------------------------------------------------- */

/* General note:
 *   stackBottom, stackLimit, and stackTop are computed from 
 *   s->currentThread->stack.  It is expected that MLton side effects these
 *   directly rather than mucking with s->currentThread->stack.  Upon entering
 *   the runtime system, the GC will update s->currentThread->stack based on
 *   these values so that everything is consistent.
 */
typedef struct GC_state {
	/* heap */
	uint fromSize;		/* size (bytes) of from space */
	pointer base;		/* start (lowest address) of from space */
	pointer limit; 		/* end of from space */
	pointer frontier; 	/* base <= frontier < limit */
	uint toSize; 		/* size (bytes) of to space */
	pointer toBase;		/* start (lowest address) of to space */
	
	/* globals */
	uint numGlobals;
	pointer *globals;	/* an array of size numGlobals */

	/* This points to a thread somewhere in the heap. */
	GC_thread currentThread;

	/* savedThread is only set while either
         *   - executing a signal handler.  It is set to the thread that was
         *     running when the signal arrived.
         *   - calling switchToThread, in which case it is set to the thread
	 *     that called switchToThread
	 */
	GC_thread savedThread;

	/* Stack in current thread */
	pointer stackTop;
	pointer stackBottom;	
	pointer stackLimit;	/* stackBottom + stackSize - maxFrameSize */
	uint maxFrameSize;
	uint maxFrameIndex;     /* 0 <= frameIndex < maxFrameIndex */
	GC_frameLayout *frameLayouts;
	/* GC_init computes frameLayout index using native codegen style. */
	bool native;
	
	/* Resizing contols (expressed as ratios of heapSize / live data) */
	uint minLive;      /* shrink heap when bytesLive * minLive < heapSize */
	uint maxLive;      /* grow heap when bytesLive * maxLive > heapSize */
	uint liveRatio;    /* when resizing, set heapSize to the smallest multiple
	* of pages greater than liveRatio * numBytesLive
	*/
	bool useFixedHeap; /* if true, then don't resize the heap */
	uint maxHeapSize;  /* if (not useFixedHeap), then s->fromSize <= maxHeapSize
			    * and s->toSize <= maxHeapSize
			    */

	/* Print out a message at the start and end of each gc. */
	bool messages;

 	/* The dfs stack is only used during the depth-first-search of an 
	 * object.  This is used in computing the size of an object.
	 * Top points at the next free space. 
         */
	pointer dfsTop;
	pointer dfsBottom;

	/* serializeStart holds the frontier at the start of the serialized
         * object during object serialization.
         */
	pointer serializeStart;

	/* only used during GC */
	int bytesLive;		/* Number of bytes copied by most recent GC. */
	pointer back;     	/* Points at next available word in toSpace. */
	pointer toLimit;	/* End of tospace. */

	/* ------------------------------------------------- */
	/*                     loadWorld                     */
	/* ------------------------------------------------- */
 	int translateDirection;	/* used by translateHeap */
	uint translateDiff;	/* used by translateHeap */
	uint magic;	/* The magic number required for a valid world file. */

	/* ------------------------------------------------- */
	/*                      Signals                      */
	/* ------------------------------------------------- */
	volatile int canHandle;	/* == 0 iff GC can switch to the signal handler
				 * thread.  This is used to implement critical
				 * sections.
				 */
	GC_thread signalHandler;/* The signal handler thread. */
	sigset_t signalsHandled;/* The signals handler expects to be handled. */
	volatile bool signalIsPending;	/* TRUE iff a signal has been received but not
				 * processed.
				 */
	sigset_t signalsPending;/* The signals that need to be handled. */
	bool inSignalHandler; 	/* TRUE iff a signal handler is running. */

	/* ------------------------------------------------- */
 	/*               gc-summary statistics               */
 	/* ------------------------------------------------- */
	bool summary; /* print a summary of gc info when the program is done */
	ullong bytesAllocated;
 	ullong bytesCopied;
 	uint numGCs; 
 	struct rusage ru_gc; /* total resource usage spent in gc */
	uint maxPause;  /* max time spent in any gc in milliseconds. */
 	uint startTime; /* the time when GC_init or GC_loadWorld is called */
	uint maxHeapSizeSeen;
	uint maxStackSizeSeen;
	uint maxBytesLive;
} *GC_state;

/* ------------------------------------------------- */
/*                  Initialization                   */
/* ------------------------------------------------- */

/* GC_init must be called before doing any allocation.
 * It must also be called before MLTON_init, GC_createStrings, and GC_createIntInfs.
 * Before calling GC_init, you must initialize:
 *   numGlobals
 *   globals 
 *   maxFrameSize
 *   maxFrameIndex
 *   frameLayouts
 *   native
 *   useFixedHeap
 * if (useFixedHeap)
 *   then fromSize should be set to the semispace size
 *   else fromSize be set to the initial amount of live data that will be placed
 *          into the heap (e.g. with GC_createStrings).  The initial heap size will
 *          be set to fromSize * s->liveRatio.
 *        maxHeapSize should be set to 0 if you want it to be figured out
 *          automatically, otherwise set it to what you want.
 */
void GC_init(GC_state s);

struct GC_stringInit {
  uint globalIndex;
  char *str;
  uint size;
};

/*  The inits array should be NULL terminated.
 *  I.E. the final element should be {0, NULL, 0}.
 */
void GC_createStrings(GC_state s, struct GC_stringInit inits[]);

/*
 * The function fixedGetrusage() works just like getrusage() except
 * that it actually works.  I.e., it does not suffer from the Linux
 * kernel bugs associated with the user and system times.
 */
int fixedGetrusage(int who, struct rusage *rup);

/* ------------------------------------------------- */
/*                      GC_done                      */
/* ------------------------------------------------- */

/* GC_done should be called after the program is done.
 * munmaps heap and stack.
 * Prints out gc statistics if s->summary is set.
 */
void GC_done (GC_state s);

/* ------------------------------------------------- */
/*                       GC_gc                       */
/* ------------------------------------------------- */

void GC_doGC (GC_state s, uint bytesRequested, uint stackBytesRequested);
void GC_enter (GC_state s);

/* Do a gc.
 * This will also resize the stack if necessary.
 * It will also switch to the signal handler thread if there is a pending signal.
 */
void GC_gc (GC_state s, uint bytesRequested, bool force,
		string file, int line);

/* ------------------------------------------------- */
/*                 GC_finishHandler                  */
/* ------------------------------------------------- */

/* This should be called */
void GC_finishHandler (GC_state s, GC_thread t);

/* ------------------------------------------------- */
/*                      GC_size                      */
/* ------------------------------------------------- */

/* Return the amount of heap space taken by the object pointed to by root. */
uint GC_size (GC_state s, pointer root);

/* ------------------------------------------------- */
/*                   Serialization                   */
/* ------------------------------------------------- */

/* Return a serialized version of the object rooted at root. */
/* pointer GC_serialize(GC_state s, pointer root); */

/* Return the deserialization of the word8vector pointed to by pointer */
/* pointer GC_deserialize(GC_state s, pointer word8vector); */

/* ------------------------------------------------- */
/*                      Arrays                       */
/* ------------------------------------------------- */

/* The array size is stored before the header */
static inline uint* GC_arrayNumElementsp(pointer a) {
	return ((uint*)a - 2);
}

static inline int GC_arrayNumElements(pointer a) {
	return *(GC_arrayNumElementsp(a));
}

static inline void GC_arrayShrink(pointer array, uint numElements) {
	*GC_arrayNumElementsp(array) = numElements;
}

/* ------------------------------------------------- */
/*                      Threads                      */
/* ------------------------------------------------- */


/* Both copyThread and copyThreadShrink place the copy in s->savedThread. */
void GC_copyThread(GC_state s, GC_thread t);
void GC_copyThreadShrink(GC_state s, GC_thread t);
void GC_threadSwitchTo(GC_state s, GC_thread t);

/* ------------------------------------------------- */
/*                      Worlds                       */
/* ------------------------------------------------- */

/* GC_saveWorld should be called in a child process, because it exits when done.
 */
void GC_saveWorld(GC_state s, 
			pointer fileName,
			void (*saveGlobals)(FILE *file));

void GC_loadWorld(GC_state s, 
			char *fileName,
			void (*loadGlobals)(FILE *file));

/* ------------------------------------------------- */
/*                    GC_handler                     */
/* ------------------------------------------------- */

/* This is the baked-in signal handler.  It causes the next limit check to fail.
 */
void GC_handler(GC_state s, int signum);

/* ------------------------------------------------- */
/*                       Misc                        */
/* ------------------------------------------------- */

static inline bool GC_isValidFrontier(GC_state s, pointer frontier) {
	return s->base <= frontier and frontier <= s->limit;
}

static inline bool GC_isValidSlot(GC_state s, pointer slot) {
	return s->stackBottom <= slot 
		and slot < s->stackBottom + s->currentThread->stack->reserved;
}

typedef void (*GC_pointerFun)(GC_state s, pointer *p);

/* Apply f to each global pointer into the heap. */
void GC_foreachGlobal(GC_state s, GC_pointerFun f);
void GC_foreachPointerInRange(GC_state s, pointer front, pointer *back,
					GC_pointerFun f);
void GC_initCounters(GC_state s);
void GC_setHeapParams(GC_state s, uint size);

/* GC_computeHeapSize returns min(s->maxHeapSize, live * ratio).
 * It is careful not to overflow when doing the multiply.
 * It should only be called when not s->useFixedHeap, since s->maxHeapSize is
 * only set in that case.
 */
uint GC_computeHeapSize(GC_state s, uint live, uint ratio);

bool GC_mutatorInvariant(GC_state s);

void GC_fromSpace(GC_state s);
void GC_toSpace(GC_state s);
void GC_setStack(GC_state s);

uint GC_objectSize(pointer p);

pointer GC_foreachPointerInObject(GC_state s, GC_pointerFun f, pointer p);

/* Return a pointer to the header for the object pointed to by p. */
static inline word* GC_getHeaderp(pointer p) {
	return (word*)(p - WORD_SIZE);
}

/* Return the header for the object pointed to by p. */
static inline word GC_getHeader(pointer p) {
	return *(GC_getHeaderp(p));
}

#endif /* #ifndef _MLTON_GC_H */
