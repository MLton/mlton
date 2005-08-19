/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "platform.h"

/* The mutator should maintain the invariants
 *
 *  function entry: stackTop + maxFrameSize <= endOfStack
 *  anywhere else: stackTop + 2 * maxFrameSize <= endOfStack
 * 
 * The latter will give it enough space to make a function call and always
 * satisfy the former.  The former will allow it to make a gc call at the
 * function entry limit.
 */

#ifndef DEBUG
#define DEBUG FALSE
#endif

#ifndef DEBUG_PROFILE
#define	DEBUG_PROFILE FALSE
#endif

enum {
	BOGUS_EXN_STACK = 0xFFFFFFFF,
	CARD_SIZE_LOG2 = 8, /* must agree w/ cardSizeLog2 in ssa-to-rssa.fun */
	COPY_CHUNK_SIZE = 0x2000000, /* 32M */
	CROSS_MAP_EMPTY = 255,
	CURRENT_SOURCE_UNDEFINED = 0xFFFFFFFF,
	DEBUG_ARRAY = FALSE,
	DEBUG_CALL_STACK = FALSE,
	DEBUG_CARD_MARKING = FALSE,
	DEBUG_DETAILED = FALSE,
	DEBUG_ENTER_LEAVE = FALSE,
	DEBUG_GENERATIONAL = FALSE,
	DEBUG_MARK_COMPACT = FALSE,
	DEBUG_RESIZING = FALSE,
	DEBUG_SHARE = FALSE,
	DEBUG_SIZE = FALSE,
	DEBUG_STACKS = FALSE,
	DEBUG_THREADS = FALSE,
	DEBUG_WEAK = FALSE,
	DEBUG_WORLD = FALSE,
	FORCE_GENERATIONAL = FALSE,
	FORCE_MARK_COMPACT = FALSE,
	FORWARDED = 0xFFFFFFFF,
	STACK_HEADER_SIZE = WORD_SIZE,
};

typedef enum {
	MARK_MODE,
	UNMARK_MODE,
} MarkMode;

#define EMPTY_HEADER GC_objectHeader (EMPTY_TYPE_INDEX)
#define STACK_HEADER GC_objectHeader (STACK_TYPE_INDEX)
#define STRING_HEADER GC_objectHeader (STRING_TYPE_INDEX)
#define THREAD_HEADER GC_objectHeader (THREAD_TYPE_INDEX)
#define WEAK_GONE_HEADER GC_objectHeader (WEAK_GONE_TYPE_INDEX)
#define WORD8_VECTOR_HEADER GC_objectHeader (WORD8_TYPE_INDEX)

#define SPLIT_HEADER()								\
	do {									\
		int objectTypeIndex;						\
		GC_ObjectType *t;						\
										\
		assert (1 == (header & 1));					\
		objectTypeIndex = (header & TYPE_INDEX_MASK) >> 1;		\
		assert (0 <= objectTypeIndex					\
				and objectTypeIndex < s->objectTypesSize);	\
		t = &s->objectTypes [objectTypeIndex];				\
		tag = t->tag;							\
		hasIdentity = t->hasIdentity;					\
		numNonPointers = t->numNonPointers;				\
		numPointers = t->numPointers;					\
		if (DEBUG_DETAILED)						\
			fprintf (stderr, "SPLIT_HEADER (0x%08x)  numNonPointers = %u  numPointers = %u\n", \
					(uint)header, numNonPointers, numPointers);	\
	} while (0)

static char* tagToString (GC_ObjectTypeTag t) {
	switch (t) {
	case ARRAY_TAG:
	return "ARRAY";
	case NORMAL_TAG:
	return "NORMAL";
	case STACK_TAG:
	return "STACK";
	case WEAK_TAG:
	return "WEAK";
	default:
	die ("bad tag %u", t);
	}
}

static inline ulong meg (uint n) {
	return n / (1024ul * 1024ul);
}

static inline uint toBytes (uint n) {
	return n << 2;
}

static inline W64 min64 (W64 x, W64 y) {
	return ((x < y) ? x : y);
}

static inline W64 max64 (W64 x, W64 y) {
	return ((x > y) ? x : y);
}

static inline uint roundDown (uint a, uint b) {
	return a - (a % b);
}

static inline uint align (uint a, uint b) {
	assert (a >= 0);
	assert (b >= 1);
	a += b - 1;
	a -= a % b;
	return a;	
}

static inline W64 w64align (W64 a, uint b) {
	W64 res;

	assert (a >= 0);
	assert (b >= 1);
	res = a + b - 1;
	res = res - res % b;
	if (FALSE)
		fprintf (stderr, "%llu = w64Align (%llu, %u)\n", res, a, b);
	return res;
}

static bool isAligned (uint a, uint b) {
	return 0 == a % b;
}

#if ASSERT
static bool isAlignedFrontier (GC_state s, pointer p) {
	return isAligned ((uint)p + GC_NORMAL_HEADER_SIZE, s->alignment);
}

static bool isAlignedReserved (GC_state s, uint r) {
	return isAligned (STACK_HEADER_SIZE + sizeof (struct GC_stack) + r, 
				s->alignment);
}
#endif

static inline uint pad (GC_state s, uint bytes, uint extra) {
	return align (bytes + extra, s->alignment) - extra;
}

static inline pointer alignFrontier (GC_state s, pointer p) {
	return (pointer) pad (s, (uint)p, GC_NORMAL_HEADER_SIZE);
}

pointer GC_alignFrontier (GC_state s, pointer p) {
	return alignFrontier (s, p);
}

static inline uint stackReserved (GC_state s, uint r) {
	uint res;

	res = pad (s, r, STACK_HEADER_SIZE + sizeof (struct GC_stack));
	if (DEBUG_STACKS)
		fprintf (stderr, "%s = stackReserved (%s)\n",
				uintToCommaString (res),
				uintToCommaString (r));
	return res;
}

static void sunlink (char *path) {
	unless (0 == unlink (path))
		diee ("unlink (%s) failed", path);
}

/* ---------------------------------------------------------------- */
/*                    Virtual Memory Management                     */
/* ---------------------------------------------------------------- */

static inline void *GC_mmapAnon (void *start, size_t length) {
	void *res;

	res = mmapAnon (start, length);
	if (DEBUG_MEM)
		fprintf (stderr, "0x%08x = mmapAnon (0x%08x, %s)\n",
					(uint)res,
					(uint)start, 
					uintToCommaString (length));
	return res;
}

void *smmap (size_t length) {
	void *result;

	result = GC_mmapAnon (NULL, length);
	if ((void*)-1 == result) {
		showMem ();
		die ("Out of memory.");
	}
	return result;
}

static inline void GC_release (void *base, size_t length) {
	if (DEBUG_MEM)
		fprintf (stderr, "release (0x%08x, %s)\n",
				(uint)base, uintToCommaString (length));
	release (base, length);
}

static inline void GC_decommit (void *base, size_t length) {
	if (DEBUG_MEM)
		fprintf (stderr, "decommit (0x%08x, %s)\n",
				(uint)base, uintToCommaString (length));
	decommit (base, length);
}

static inline void copy (pointer src, pointer dst, uint size) {
	uint	*to,
		*from,
		*limit;

	if (DEBUG_DETAILED)
		fprintf (stderr, "copy (0x%08x, 0x%08x, %u)\n",
				(uint)src, (uint)dst, size);
	assert (isAligned ((uint)src, WORD_SIZE));
	assert (isAligned ((uint)dst, WORD_SIZE));
	assert (isAligned (size, WORD_SIZE));
	assert (dst <= src or src + size <= dst);
	if (src == dst)
		return;
	from = (uint*)src;
	to = (uint*)dst;
	limit = (uint*)(src + size);
	until (from == limit)
		*to++ = *from++;
}

/* ---------------------------------------------------------------- */
/*                              rusage                              */
/* ---------------------------------------------------------------- */

static inline void rusageZero (struct rusage *ru) {
	memset (ru, 0, sizeof (*ru));
}

static void rusagePlusMax (struct rusage *ru1,
			      struct rusage *ru2,
			      struct rusage *ru) {
	const int	million = 1000000;
	time_t		sec,
			usec;

	sec = ru1->ru_utime.tv_sec + ru2->ru_utime.tv_sec;
	usec = ru1->ru_utime.tv_usec + ru2->ru_utime.tv_usec;
	sec += (usec / million);
	usec %= million;
	ru->ru_utime.tv_sec = sec;
	ru->ru_utime.tv_usec = usec;

	sec = ru1->ru_stime.tv_sec + ru2->ru_stime.tv_sec;
	usec = ru1->ru_stime.tv_usec + ru2->ru_stime.tv_usec;
	sec += (usec / million);
	usec %= million;
	ru->ru_stime.tv_sec = sec;
	ru->ru_stime.tv_usec = usec;
}

static void rusageMinusMax (struct rusage *ru1,
				struct rusage *ru2,
				struct rusage *ru) {
	const int	million = 1000000;
	time_t		sec,
			usec;

	sec = (ru1->ru_utime.tv_sec - ru2->ru_utime.tv_sec) - 1;
	usec = ru1->ru_utime.tv_usec + million - ru2->ru_utime.tv_usec;
	sec += (usec / million);
	usec %= million;
	ru->ru_utime.tv_sec = sec;
	ru->ru_utime.tv_usec = usec;

	sec = (ru1->ru_stime.tv_sec - ru2->ru_stime.tv_sec) - 1;
	usec = ru1->ru_stime.tv_usec + million - ru2->ru_stime.tv_usec;
	sec += (usec / million);
	usec %= million;
	ru->ru_stime.tv_sec = sec;
	ru->ru_stime.tv_usec = usec;
}

static uint rusageTime (struct rusage *ru) {
	uint	result;

	result = 0;
	result += 1000 * ru->ru_utime.tv_sec;
	result += 1000 * ru->ru_stime.tv_sec;
	result += ru->ru_utime.tv_usec / 1000;
	result += ru->ru_stime.tv_usec / 1000;
	return result;
}

/* Return time as number of milliseconds. */
static uint currentTime () {
	struct rusage	ru;

	fixedGetrusage (RUSAGE_SELF, &ru);
	return rusageTime (&ru);
}

static inline void startTiming (struct rusage *ru_start) {
	fixedGetrusage (RUSAGE_SELF, ru_start);
}

static uint stopTiming (struct rusage *ru_start, struct rusage *ru_gc) {
	struct rusage ru_finish, ru_total;

	fixedGetrusage (RUSAGE_SELF, &ru_finish);
	rusageMinusMax (&ru_finish, ru_start, &ru_total);
	rusagePlusMax (ru_gc, &ru_total, ru_gc);
	return rusageTime (&ru_total);
}

/* ---------------------------------------------------------------- */
/*                            GC_display                            */
/* ---------------------------------------------------------------- */

void GC_display (GC_state s, FILE *stream) {
	fprintf (stream, "GC state\n\tcardMap = 0x%08x\n\toldGen = 0x%08x\n\toldGenSize = %s\n\toldGen + oldGenSize = 0x%08x\n\tnursery = 0x%08x\n\tfrontier = 0x%08x\n\tfrontier - nursery = %u\n\tlimitPlusSlop - frontier = %d\n",
			(uint) s->cardMap,
   			(uint) s->heap.start,
			uintToCommaString (s->oldGenSize),
			(uint) s->heap.start + s->oldGenSize,
			(uint) s->nursery, 
			(uint) s->frontier,
			s->frontier - s->nursery,
			s->limitPlusSlop - s->frontier);
	fprintf (stream, "\tcanHandle = %d\n\tsignalsIsPending = %d\n", s->canHandle, s->signalIsPending);
	fprintf (stderr, "\tcurrentThread = 0x%08x\n", (uint) s->currentThread);
	fprintf (stream, "\tstackBottom = 0x%08x\n\tstackTop - stackBottom = %u\n\tstackLimit - stackTop = %u\n",
			(uint)s->stackBottom,
			s->stackTop - s->stackBottom,
			(s->stackLimit - s->stackTop));
	fprintf (stream, "\texnStack = %u\n\tbytesNeeded = %u\n\treserved = %u\n\tused = %u\n",
			s->currentThread->exnStack,
			s->currentThread->bytesNeeded,
			s->currentThread->stack->reserved,
			s->currentThread->stack->used);
	if (DEBUG_GENERATIONAL and DEBUG_DETAILED) {
		int i;

		fprintf (stderr, "crossMap trues\n");
		for (i = 0; i < s->crossMapSize; ++i)
			unless (CROSS_MAP_EMPTY == s->crossMap[i])
				fprintf (stderr, "\t%u\n", i);
		fprintf (stderr, "\n");
	}		
}

static inline uint cardNumToSize (GC_state s, uint n) {
	return n << CARD_SIZE_LOG2;
}

static inline uint divCardSize (GC_state s, uint n) {
	return n >> CARD_SIZE_LOG2;
}

static inline pointer cardMapAddr (GC_state s, pointer p) {
	pointer res;

	res = &s->cardMapForMutator [divCardSize (s, (uint)p)];
	if (DEBUG_CARD_MARKING)
		fprintf (stderr, "0x%08x = cardMapAddr (0x%08x)\n",
				(uint)res, (uint)p);
	return res;
}

static inline bool cardIsMarked (GC_state s, pointer p) {
	return *cardMapAddr (s, p);
}

static inline void markCard (GC_state s, pointer p) {
	if (DEBUG_CARD_MARKING)
		fprintf (stderr, "markCard (0x%08x)\n", (uint)p);
	if (s->mutatorMarksCards)
		*cardMapAddr (s, p) = '\001';
}

/* ---------------------------------------------------------------- */
/*                              Stacks                              */
/* ---------------------------------------------------------------- */

/* stackSlop returns the amount of "slop" space needed between the top of 
 * the stack and the end of the stack space.
 */
static inline uint stackSlop (GC_state s) {
	return 2 * s->maxFrameSize;
}

static inline uint initialStackSize (GC_state s) {
	return stackSlop (s);
}

static inline uint stackBytes (GC_state s, uint size) {
	uint res;

	res = align (STACK_HEADER_SIZE + sizeof (struct GC_stack) + size,
			s->alignment);
	if (DEBUG_STACKS)
		fprintf (stderr, "%s = stackBytes (%s)\n",
				uintToCommaString (res),
				uintToCommaString (size));
	return res;
}

static inline pointer stackBottom (GC_state s, GC_stack stack) {
	pointer res;

	res = ((pointer)stack) + sizeof (struct GC_stack);
	assert (isAligned ((uint)res, s->alignment));
	return res;
}

/* Pointer to the topmost word in use on the stack. */
static inline pointer stackTop (GC_state s, GC_stack stack) {
	return stackBottom (s, stack) + stack->used;
}

/* Pointer to the end of stack. */
static inline pointer endOfStack (GC_state s, GC_stack stack) {
	return stackBottom (s, stack) + stack->reserved;
}

/* The maximum value stackTop may take on. */
static inline pointer stackLimit (GC_state s, GC_stack stack) {
	return endOfStack (s, stack) - stackSlop (s);
}

static inline bool stackIsEmpty (GC_stack stack) {
	return 0 == stack->used;
}

static inline uint getFrameIndex (GC_state s, word returnAddress) {
	uint res;

	res = s->returnAddressToFrameIndex (returnAddress);
	if (DEBUG_DETAILED)
		fprintf (stderr, "%u = getFrameIndex (0x%08x)\n",
				returnAddress, res);
	return res;
}

static inline uint topFrameIndex (GC_state s) {
	uint res;

	assert (s->stackTop > s->stackBottom);
	res = getFrameIndex (s, *(word*)(s->stackTop - WORD_SIZE));
	if (DEBUG_PROFILE)
		fprintf (stderr, "topFrameIndex = %u\n", res);
	return res;
}

static inline uint topFrameSourceSeqIndex (GC_state s) {
	return s->frameSources[topFrameIndex (s)];
}

static inline GC_frameLayout * getFrameLayout (GC_state s, word returnAddress) {
	GC_frameLayout *layout;
	uint index;

	index = getFrameIndex (s, returnAddress);
	if (DEBUG_DETAILED)
		fprintf (stderr, "returnAddress = 0x%08x  index = %d  frameLayoutsSize = %d\n",
				returnAddress, index, s->frameLayoutsSize);
	assert (0 <= index and index < s->frameLayoutsSize);
	layout = &(s->frameLayouts[index]);
	assert (layout->numBytes > 0);
	return layout;
}

static inline uint topFrameSize (GC_state s, GC_stack stack) {
	GC_frameLayout *layout;
	
	assert (not (stackIsEmpty (stack)));
	layout = getFrameLayout (s, *(word*)(stackTop (s, stack) - WORD_SIZE));
	return layout->numBytes;
}

static inline uint stackNeedsReserved (GC_state s, GC_stack stack) {
	return stack->used + stackSlop (s) - topFrameSize (s, stack);
}

#if ASSERT
static bool hasBytesFree (GC_state s, W32 oldGen, W32 nursery) {
	bool res;

	res = s->oldGenSize + oldGen 
			+ (s->canMinor ? 2 : 1) 
				* (s->limitPlusSlop - s->nursery)
			<= s->heap.size
		and nursery <= s->limitPlusSlop - s->frontier;
	if (DEBUG_DETAILED)
		fprintf (stderr, "%s = hasBytesFree (%s, %s)\n",
				boolToString (res),
				uintToCommaString (oldGen),
				uintToCommaString (nursery));
	return res;
}
#endif

/* bytesRequested includes the header. */
static pointer object (GC_state s, uint header, W32 bytesRequested,
				bool allocInOldGen,
				Bool hasDouble) {
	pointer frontier;
	pointer result;

	if (DEBUG)
		fprintf (stderr, "object (0x%08x, %u, %s)\n",
				header, 
				(uint)bytesRequested,
				boolToString (allocInOldGen));
	assert (isAligned (bytesRequested, s->alignment));
	assert (allocInOldGen
			? hasBytesFree (s, bytesRequested, 0)
			: hasBytesFree (s, 0, bytesRequested));
	if (allocInOldGen) {
		frontier = s->heap.start + s->oldGenSize;
		s->oldGenSize += bytesRequested;
		s->bytesAllocated += bytesRequested;
	} else {
		if (DEBUG_DETAILED)
			fprintf (stderr, "frontier changed from 0x%08x to 0x%08x\n",
					(uint)s->frontier, 
					(uint)(s->frontier + bytesRequested));
		frontier = s->frontier;
		s->frontier += bytesRequested;
	}
	GC_profileAllocInc (s, bytesRequested);
	*(uint*)(frontier) = header;
	result = frontier + GC_NORMAL_HEADER_SIZE;
	return result;
}

static GC_stack newStack (GC_state s, uint reserved, bool allocInOldGen) {
	GC_stack stack;

	reserved = stackReserved (s, reserved);
	if (reserved > s->maxStackSizeSeen)
		s->maxStackSizeSeen = reserved;
	stack = (GC_stack) object (s, STACK_HEADER, stackBytes (s, reserved),
					allocInOldGen, TRUE);
	stack->reserved = reserved;
	stack->used = 0;
	if (DEBUG_STACKS)
		fprintf (stderr, "0x%x = newStack (%u)\n", (uint)stack, 
				reserved);
	return stack;
}

static void setStack (GC_state s) {
	GC_stack stack;

	s->exnStack = s->currentThread->exnStack;
	stack = s->currentThread->stack;
	s->stackBottom = stackBottom (s, stack);
	s->stackTop = stackTop (s, stack);
	s->stackLimit = stackLimit (s, stack);
	/* We must card mark the stack because it will be updated by the mutator.
	 */
	markCard (s, (pointer)stack);
}

static void stackCopy (GC_state s, GC_stack from, GC_stack to) {
	assert (from->used <= to->reserved);
	to->used = from->used;
	if (DEBUG_STACKS)
		fprintf (stderr, "stackCopy from 0x%08x to 0x%08x of length %u\n",
				(uint) stackBottom (s, from), 
				(uint) stackBottom (s, to),
				from->used);
	memcpy (stackBottom (s, to), stackBottom (s, from), from->used);
}

/* Number of bytes used by the stack. */
static inline uint currentStackUsed (GC_state s) {
	return s->stackTop - s->stackBottom;
}

/* ---------------------------------------------------------------- */
/*                          foreachGlobal                           */
/* ---------------------------------------------------------------- */

typedef void (*GC_pointerFun) (GC_state s, pointer *p);

static inline void maybeCall (GC_pointerFun f, GC_state s, pointer *pp) {
	if (GC_isPointer (*pp))
		f (s, pp);
}

/* Apply f to each global pointer into the heap. */
static inline void foreachGlobal (GC_state s, GC_pointerFun f) {
	int i;

 	for (i = 0; i < s->globalsSize; ++i) {
		if (DEBUG_DETAILED)
			fprintf (stderr, "foreachGlobal %u\n", i);
		maybeCall (f, s, &s->globals [i]);
	}
	if (DEBUG_DETAILED)
		fprintf (stderr, "foreachGlobal threads\n");
	maybeCall (f, s, (pointer*)&s->callFromCHandler);
	maybeCall (f, s, (pointer*)&s->currentThread);
	maybeCall (f, s, (pointer*)&s->savedThread);
	maybeCall (f, s, (pointer*)&s->signalHandler);
}

#if ASSERT
static pointer arrayPointer (GC_state s, 
				pointer a, 
				uint arrayIndex, 
				uint pointerIndex) {
	Bool hasIdentity;
	word header;
	uint numPointers;
	uint numNonPointers;
	uint tag;

	header = GC_getHeader (a);
	SPLIT_HEADER();
	assert (tag == ARRAY_TAG);
	return a 
		+ arrayIndex * (numNonPointers + toBytes (numPointers))
		+ numNonPointers
		+ pointerIndex * POINTER_SIZE;
}
#endif

/* The number of bytes in an array, not including the header. */
static inline uint arrayNumBytes (GC_state s,
					pointer p, 
					uint numPointers,
					uint numNonPointers) {
	uint bytesPerElement;
	uint numElements;
	uint result;
	
	numElements = GC_arrayNumElements (p);
	bytesPerElement = numNonPointers + toBytes (numPointers);
	result = numElements * bytesPerElement;
	/* Empty arrays have POINTER_SIZE bytes for the forwarding pointer */
	if (0 == result) 
		result = POINTER_SIZE;
	return pad (s, result, GC_ARRAY_HEADER_SIZE);
}

/* ---------------------------------------------------------------- */
/*                      foreachPointerInObject                      */
/* ---------------------------------------------------------------- */
/* foreachPointerInObject (s, p,f, ws) applies f to each pointer in the object
 * pointer to by p.
 * Returns pointer to the end of object, i.e. just past object.
 *
 * If ws, then the object pointer in weak objects is skipped.
 */

static inline pointer foreachPointerInObject (GC_state s, pointer p,
						Bool skipWeaks,
						GC_pointerFun f) {
	Bool hasIdentity;
	word header;
	uint numPointers;
	uint numNonPointers;
	uint tag;

	header = GC_getHeader (p);
	SPLIT_HEADER();
	if (DEBUG_DETAILED)
		fprintf (stderr, "foreachPointerInObject p = 0x%x  header = 0x%x  tag = %s  numNonPointers = %d  numPointers = %d\n", 
			(uint)p, header, tagToString (tag), 
			numNonPointers, numPointers);
	if (NORMAL_TAG == tag) {
		pointer max;

		p += toBytes (numNonPointers);
		max = p + toBytes (numPointers);
		/* Apply f to all internal pointers. */
		for ( ; p < max; p += POINTER_SIZE) {
			if (DEBUG_DETAILED)
				fprintf (stderr, "p = 0x%08x  *p = 0x%08x\n",
						(uint)p, *(uint*)p);
			maybeCall (f, s, (pointer*)p);
		}
	} else if (WEAK_TAG == tag) {
		if (not skipWeaks and 1 == numPointers)
			maybeCall (f, s, (pointer*)&(((GC_weak)p)->object));
		p += sizeof (struct GC_weak);
	} else if (ARRAY_TAG == tag) {
		uint bytesPerElement;
		uint dataBytes;
		pointer max;
		uint numElements;

		numElements = GC_arrayNumElements (p);
		bytesPerElement = numNonPointers + toBytes (numPointers);
		dataBytes = numElements * bytesPerElement;
		/* Must check 0 == dataBytes before 0 == numPointers to correctly
		 * handle arrays when both are true.
		 */
		if (0 == dataBytes)
			/* Empty arrays have space for forwarding pointer. */
			dataBytes = POINTER_SIZE;
		else if (0 == numPointers)
			/* No pointers to process. */
			;
		else {
			max = p + dataBytes;
			if (0 == numNonPointers)
			  	/* Array with only pointers. */
				for (; p < max; p += POINTER_SIZE)
					maybeCall (f, s, (pointer*)p);
			else {
				/* Array with a mix of pointers and non-pointers.
 			         */
				uint numBytesPointers;
			
				numBytesPointers = toBytes (numPointers);
				/* For each array element. */
				while (p < max) {
					pointer max2;

					/* Skip the non-pointers. */
					p += numNonPointers;
					max2 = p + numBytesPointers;
					/* For each internal pointer. */
					for ( ; p < max2; p += POINTER_SIZE) 
						maybeCall (f, s, (pointer*)p);
				}
			}
			assert (p == max);
			p -= dataBytes;
		}
		p += pad (s, dataBytes, GC_ARRAY_HEADER_SIZE);
	} else { /* stack */
		GC_stack stack;
		pointer top, bottom;
		int i;
		word returnAddress;
		GC_frameLayout *layout;
		GC_offsets frameOffsets;

		assert (STACK_TAG == tag);
		stack = (GC_stack)p;
		bottom = stackBottom (s, stack);
		top = stackTop (s, stack);
		assert (stack->used <= stack->reserved);
		while (top > bottom) {
			/* Invariant: top points just past a "return address". */
			returnAddress = *(word*) (top - WORD_SIZE);
			if (DEBUG) {
				fprintf (stderr, "  top = %d  return address = ",
						top - bottom);
				fprintf (stderr, "0x%08x.\n", returnAddress);
			}
			layout = getFrameLayout (s, returnAddress); 
			frameOffsets = layout->offsets;
			top -= layout->numBytes;
			for (i = 0 ; i < frameOffsets[0] ; ++i) {
				if (DEBUG)
					fprintf(stderr, 
						"    offset %u  address 0x%08x\n", 
						frameOffsets[i + 1],
						(uint)(*(pointer*)(top + frameOffsets[i + 1])));
				maybeCall(f, s, 
					  (pointer*)
					  (top + frameOffsets[i + 1]));
			}
		}
		assert(top == bottom);
		p += sizeof (struct GC_stack) + stack->reserved;
	}
	return p;
}

/* ---------------------------------------------------------------- */
/*                              toData                              */
/* ---------------------------------------------------------------- */

/* If p points at the beginning of an object, then toData p returns a pointer 
 * to the start of the object data.
 */
static inline pointer toData (GC_state s, pointer p) {
	word header;
	pointer res;

	assert (isAlignedFrontier (s, p));
	header = *(word*)p;
	if (0 == header)
		/* Looking at the counter word in an array. */
		res = p + GC_ARRAY_HEADER_SIZE;
	else
		/* Looking at a header word. */
		res = p + GC_NORMAL_HEADER_SIZE;
	assert (isAligned ((uint)res, s->alignment));
	return res;
}

/* ---------------------------------------------------------------- */
/*                      foreachPointerInRange                       */
/* ---------------------------------------------------------------- */

/* foreachPointerInRange (s, front, back, ws, f)
 * Apply f to each pointer between front and *back, which should be a 
 * contiguous sequence of objects, where front points at the beginning of
 * the first object and *back points just past the end of the last object.
 * f may increase *back (for example, this is done by forward).
 * foreachPointerInRange returns a pointer to the end of the last object it
 * visits.
 *
 * If ws, then the object pointer in weak objects is skipped.
 */

static inline pointer foreachPointerInRange (GC_state s, 
						pointer front, 
						pointer *back,
						Bool skipWeaks,
						GC_pointerFun f) {
	pointer b;

	assert (isAlignedFrontier (s, front));
	if (DEBUG_DETAILED)
		fprintf (stderr, "foreachPointerInRange  front = 0x%08x  *back = 0x%08x\n",
				(uint)front, *(uint*)back);
	b = *back;
	assert (front <= b);
 	while (front < b) {
		while (front < b) {
			assert (isAligned ((uint)front, WORD_SIZE));
	       		if (DEBUG_DETAILED)
				fprintf (stderr, "front = 0x%08x  *back = 0x%08x\n",
						(uint)front, *(uint*)back);
			front = foreachPointerInObject 
					(s, toData (s, front), skipWeaks, f);
		}
		b = *back;
	}
	return front;
}

/* ---------------------------------------------------------------- */
/*                            invariant                             */
/* ---------------------------------------------------------------- */

static bool mutatorFrontierInvariant (GC_state s) {
	return (s->currentThread->bytesNeeded <= 
			s->limitPlusSlop - s->frontier);
}

static bool mutatorStackInvariant (GC_state s) {
	return (stackTop (s, s->currentThread->stack) <= 
			stackLimit (s, s->currentThread->stack) + 
			topFrameSize (s, s->currentThread->stack));
}

static bool ratiosOk (GC_state s) {
	return 1.0 < s->growRatio
			and 1.0 < s->nurseryRatio
			and 1.0 < s->markCompactRatio
			and s->markCompactRatio <= s->copyRatio
			and s->copyRatio <= s->liveRatio;
}

static inline bool isInNursery (GC_state s, pointer p) {
	return s->nursery <= p and p < s->frontier;
}

#if ASSERT

static inline bool isInOldGen (GC_state s, pointer p) {
	return s->heap.start <= p and p < s->heap.start + s->oldGenSize;
}

static inline bool isInFromSpace (GC_state s, pointer p) {
 	return (isInOldGen (s, p) or isInNursery (s, p));
}

static inline void assertIsInFromSpace (GC_state s, pointer *p) {
#if ASSERT
	unless (isInFromSpace (s, *p))
		die ("gc.c: assertIsInFromSpace p = 0x%08x  *p = 0x%08x);\n",
			(uint)p, *(uint*)p);
	/* The following checks that intergenerational pointers have the
	 * appropriate card marked.  Unfortunately, it doesn't work because
	 * for stacks, the card containing the beginning of the stack is marked,
	 * but any remaining cards aren't.
	 */
	if (FALSE and s->mutatorMarksCards 
		and isInOldGen (s, (pointer)p) 
 		and isInNursery (s, *p)
		and not cardIsMarked (s, (pointer)p)) {
		GC_display (s, stderr);
		die ("gc.c: intergenerational pointer from 0x%08x to 0x%08x with unmarked card.\n",
			(uint)p, *(uint*)p);
	}
#endif
}

static inline bool isInToSpace (GC_state s, pointer p) {
	return (not (GC_isPointer (p))
			or (s->toSpace <= p and p < s->toLimit));
}

static bool invariant (GC_state s) {
	int i;
	pointer back;
	GC_stack stack;

	if (DEBUG)
		fprintf (stderr, "invariant\n");
	assert (ratiosOk (s));
	/* Frame layouts */
	for (i = 0; i < s->frameLayoutsSize; ++i) {
		GC_frameLayout *layout;

		layout = &(s->frameLayouts[i]);
		if (layout->numBytes > 0) {
			GC_offsets offsets;
//			int j;

			assert (layout->numBytes <= s->maxFrameSize);
			offsets = layout->offsets;
// No longer correct, since handler frames have a "size" (i.e. return address)
// pointing into the middle of the frame.
//			for (j = 0; j < offsets[0]; ++j)
//				assert (offsets[j + 1] < layout->numBytes);
		}
	}
	if (s->mutatorMarksCards) {
		assert (s->cardMap == 
				&s->cardMapForMutator[divCardSize(s, (uint)s->heap.start)]);
		assert (&s->cardMapForMutator[divCardSize (s, (uint)s->heap.start + s->heap.size - WORD_SIZE)]
				< s->cardMap + s->cardMapSize);
	}
	/* Heap */
	assert (isAligned (s->heap.size, s->pageSize));
	assert (isAligned ((uint)s->heap.start, s->cardSize));
	assert (isAlignedFrontier (s, s->heap.start + s->oldGenSize));
	assert (isAlignedFrontier (s, s->nursery));
	assert (isAlignedFrontier (s, s->frontier));
	assert (s->nursery <= s->frontier);
	unless (0 == s->heap.size) {
		assert (s->nursery <= s->frontier);
		assert (s->frontier <= s->limitPlusSlop);
		assert (s->limit == s->limitPlusSlop - LIMIT_SLOP);
		assert (hasBytesFree (s, 0, 0));
	}
	assert (s->heap2.start == NULL or s->heap.size == s->heap2.size);
	/* Check that all pointers are into from space. */
	foreachGlobal (s, assertIsInFromSpace);
	back = s->heap.start + s->oldGenSize;
	if (DEBUG_DETAILED)
		fprintf (stderr, "Checking old generation.\n");
	foreachPointerInRange (s, alignFrontier (s, s->heap.start), &back, FALSE,
				assertIsInFromSpace);
	if (DEBUG_DETAILED)
		fprintf (stderr, "Checking nursery.\n");
	foreachPointerInRange (s, s->nursery, &s->frontier, FALSE,
				assertIsInFromSpace);
	/* Current thread. */
	stack = s->currentThread->stack;
	assert (isAlignedReserved (s, stack->reserved));
	assert (s->stackBottom == stackBottom (s, stack));
	assert (s->stackTop == stackTop (s, stack));
 	assert (s->stackLimit == stackLimit (s, stack));
	assert (stack->used == currentStackUsed (s));
	assert (stack->used <= stack->reserved);
 	assert (s->stackBottom <= s->stackTop);
	if (DEBUG)
		fprintf (stderr, "invariant passed\n");
	return TRUE;
}

static bool mutatorInvariant (GC_state s, bool frontier, bool stack) {
	if (DEBUG)
		GC_display (s, stderr);
	if (frontier)
		assert (mutatorFrontierInvariant(s));
	if (stack)
		assert (mutatorStackInvariant(s));
	assert (invariant (s));
	return TRUE;
}
#endif /* #if ASSERT */

/* ---------------------------------------------------------------- */
/*                         enter and leave                          */
/* ---------------------------------------------------------------- */

static inline void atomicBegin (GC_state s) {
	s->canHandle++;
	if (0 == s->limit)
		s->limit = s->limitPlusSlop - LIMIT_SLOP;
}

static inline void atomicEnd (GC_state s) {
	s->canHandle--;
	if (0 == s->canHandle and s->signalIsPending)
		s->limit = 0;
}

/* enter and leave should be called at the start and end of every GC function
 * that is exported to the outside world.  They make sure that the function
 * is run in a critical section and check the GC invariant.
 */
static void enter (GC_state s) {
	if (DEBUG)
		fprintf (stderr, "enter\n");
	/* used needs to be set because the mutator has changed s->stackTop. */
	s->currentThread->stack->used = currentStackUsed (s);
	s->currentThread->exnStack = s->exnStack;
	if (DEBUG) 
		GC_display (s, stderr);
	atomicBegin (s);
	assert (invariant (s));
	if (DEBUG)
		fprintf (stderr, "enter ok\n");
}

static void leave (GC_state s) {
	if (DEBUG)
		fprintf (stderr, "leave\n");
	/* The mutator frontier invariant may not hold
	 * for functions that don't ensureBytesFree.
	 */
	assert (mutatorInvariant (s, FALSE, TRUE));
	atomicEnd (s);
	if (DEBUG)
		fprintf (stderr, "leave ok\n");
}

/* ---------------------------------------------------------------- */
/*                              Heaps                               */
/* ---------------------------------------------------------------- */

/* heapDesiredSize (s, l, c) returns the desired heap size for a heap with
 * l bytes live, given that the current heap size is c.
 */
static W32 heapDesiredSize (GC_state s, W64 live, W32 currentSize) {
	W32 res;
	float ratio;

	ratio = (float)s->ram / (float)live;
        if (ratio >= s->liveRatio + s->growRatio) {
		/* Cheney copying fits in RAM with desired liveRatio. */
		res = live * s->liveRatio;
		/* If the heap is currently close in size to what we want, leave
		 * it alone.  Favor growing over shrinking.
		 */
		unless (res >= 1.1 * currentSize 
				or res <= .5 * currentSize)
			res = currentSize;
	} else if (s->growRatio >= s->copyRatio
			and ratio >= 2 * s->copyRatio) {
		/* Split RAM in half.  Round down by pageSize so that the total
		 * amount of space taken isn't greater than RAM once rounding
		 * happens.  This is so resizeHeap2 doesn't get confused and
		 * free a semispace in a misguided attempt to avoid paging.
		 */
		res = roundDown (s->ram / 2, s->pageSize) ;
	} else if (ratio >= s->copyRatio + s->growRatio) {
		/* Cheney copying fits in RAM. */
		res = s->ram - s->growRatio * live;
		/* If the heap isn't too much smaller than what we want, leave
		 * it alone.  On the other hand, if it is bigger we want to
		 * leave res as is so that the heap is shrunk, to try to avoid
		 * paging.
		 */
		if (0.9 * res <= currentSize and currentSize <= res)
			res = currentSize;
	} else if (ratio >= s->markCompactRatio) {
		/* Mark compact fits in ram.  It doesn't matter what the current
		 * size is.  If the heap is currently smaller, we are using
		 * copying and should switch to mark-compact.  If the heap is
		 * currently bigger, we want to shrink back to ram size to avoid
		 * paging.
		 */
		res = s->ram;
	} else { /* Required live ratio. */
		res = live * s->markCompactRatio;
		/* If the current heap is bigger than res, the shrinking always
		 * sounds like a good idea.  However, depending on what pages
		 * the VM keeps around, growing could be very expensive, if it
		 * involves paging the entire heap.  Hopefully the copy loop
		 * in growFromSpace will make the right thing happen.
		 */ 
	}
	if (s->fixedHeap > 0) {
		if (res > s->fixedHeap / 2)
			res = s->fixedHeap;
		else
			res = s->fixedHeap / 2;
		if (res < live)
			die ("Out of memory with fixed heap size %s.",
				uintToCommaString (s->fixedHeap));
	} else if (s->maxHeap > 0) {
		if (res > s->maxHeap)
			res = s->maxHeap;
		if (res < live)
			die ("Out of memory with max heap size %s.",
				uintToCommaString (s->maxHeap));
	}
	if (DEBUG_RESIZING)
		fprintf (stderr, "%s = heapDesiredSize (%s)\n",
				uintToCommaString (res),
				ullongToCommaString (live));
	assert (res >= live);
	return res;
}

static inline void heapInit (GC_heap h) {
	h->size = 0;
	h->start = NULL;
}

static inline bool heapIsInit (GC_heap h) {
	return 0 == h->size;
}

static void heapRelease (GC_state s, GC_heap h) {
	if (NULL == h->start)
		return;
	if (DEBUG or s->messages)
		fprintf (stderr, "Releasing heap at 0x%08x of size %s.\n", 
				(uint)h->start, 
				uintToCommaString (h->size));
	GC_release (h->start, h->size);
	heapInit (h);
}

static void heapShrink (GC_state s, GC_heap h, W32 keep) {
	assert (keep <= h->size);
	if (0 == keep) {
		heapRelease (s, h);
		return;
	}
	keep = align (keep, s->pageSize);
	if (keep < h->size) {
		if (DEBUG or s->messages)
			fprintf (stderr, 
				"Shrinking heap at 0x%08x of size %s to %s bytes.\n",
				(uint)h->start, 
				uintToCommaString (h->size),
				uintToCommaString (keep));
		GC_decommit (h->start + keep, h->size - keep);
		h->size = keep;
	}
}

static void clearCardMap (GC_state s) {
	memset (s->cardMap, 0, s->cardMapSize);
}

static void setNursery (GC_state s, W32 oldGenBytesRequested,
				W32 nurseryBytesRequested) {
	GC_heap h;
	uint nurserySize;

	if (DEBUG_DETAILED)
		fprintf (stderr, "setNursery.  oldGenBytesRequested = %s  frontier = 0x%08x\n",  
				uintToCommaString (oldGenBytesRequested),
				(uint)s->frontier);
	h = &s->heap;
	assert (isAlignedFrontier (s, h->start + s->oldGenSize 
					+ oldGenBytesRequested));
	nurserySize = h->size - s->oldGenSize - oldGenBytesRequested;
	s->limitPlusSlop = h->start + h->size;
	s->limit = s->limitPlusSlop - LIMIT_SLOP;
	assert (isAligned (nurserySize, WORD_SIZE));
	if (	/* The mutator marks cards. */
       		s->mutatorMarksCards
		/* There is enough space in the nursery. */
		and (nurseryBytesRequested 
			<= s->limitPlusSlop
				- alignFrontier (s, s->limitPlusSlop
							- nurserySize/2 + 2))
		/* The nursery is large enough to be worth it. */
		and (((float)(h->size - s->bytesLive) 
			/ (float)nurserySize) <= s->nurseryRatio)
		and /* There is a reason to use generational GC. */
		(
		/* We must use it for debugging pruposes. */
		FORCE_GENERATIONAL
		/* We just did a mark compact, so it will be advantageous to
		 * to use it.
		 */
		or (s->lastMajor == GC_MARK_COMPACT)
		/* The live ratio is low enough to make it worthwhile. */
		or (float)h->size / (float)s->bytesLive 
			<= (h->size < s->ram
				? s->copyGenerationalRatio
				: s->markCompactGenerationalRatio)
		)) {
		s->canMinor = TRUE;
		nurserySize /= 2;
		unless (isAligned (nurserySize, WORD_SIZE))
			nurserySize -= 2;
		clearCardMap (s);
	} else {
		unless (nurseryBytesRequested 
				<= s->limitPlusSlop
					- alignFrontier (s, s->limitPlusSlop
								- nurserySize))
			die ("Out of memory.  Insufficient space in nursery.");
		s->canMinor = FALSE;
	}
	assert (nurseryBytesRequested 
			<= s->limitPlusSlop
				- alignFrontier (s, s->limitPlusSlop 
							- nurserySize));
	s->nursery = alignFrontier (s, s->limitPlusSlop - nurserySize);
	s->frontier = s->nursery;
	assert (nurseryBytesRequested <= s->limitPlusSlop - s->frontier);
	assert (isAlignedFrontier (s, s->nursery));
	assert (hasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
}

static inline void clearCrossMap (GC_state s) {
	if (DEBUG_GENERATIONAL and DEBUG_DETAILED)
		fprintf (stderr, "clearCrossMap ()\n");
	s->crossMapValidSize = 0;
	memset (s->crossMap, CROSS_MAP_EMPTY, s->crossMapSize);
}

static void setCardMapForMutator (GC_state s) {
	unless (s->mutatorMarksCards)
		return;
	/* It's OK if the subtraction below underflows because all the 
         * subsequent additions to mark the cards will overflow and put us
	 * in the right place.
         */
	s->cardMapForMutator = s->cardMap - divCardSize (s, (uint)s->heap.start);
	if (DEBUG_CARD_MARKING)
		fprintf (stderr, "cardMapForMutator = 0x%08x\n",
				(uint)s->cardMapForMutator);
}

static void createCardMapAndCrossMap (GC_state s) {
	GC_heap h;

	unless (s->mutatorMarksCards) {
		s->cardMapSize = 0;
		s->cardMap = NULL;
		s->cardMapForMutator = NULL;
		s->crossMapSize = 0;
		s->crossMap = NULL;
		return;
	}
	h = &s->heap;
	assert (isAligned (h->size, s->cardSize));
	s->cardMapSize = align (divCardSize (s, h->size), s->pageSize);
	s->crossMapSize = s->cardMapSize;
	if (DEBUG_MEM)
		fprintf (stderr, "Creating card/cross map of size %s\n",
				uintToCommaString
					(s->cardMapSize + s->crossMapSize));
	s->cardMap = smmap (s->cardMapSize + s->crossMapSize);
	s->crossMap = (uchar *)s->cardMap + s->cardMapSize;
	if (DEBUG_CARD_MARKING)
		fprintf (stderr, "cardMap = 0x%08x  crossMap = 0x%08x\n", 
				(uint)s->cardMap,
				(uint)s->crossMap);
	setCardMapForMutator (s);
	clearCrossMap (s);
}

/* heapCreate (s, h, need, minSize) allocates a heap of the size necessary to
 * work with need live data, and ensures that at least minSize is available.
 * It returns TRUE if it is able to allocate the space, and returns FALSE if it
 * is unable.  If a reasonable size to space is already there, then heapCreate
 * leaves it.
 */
static bool heapCreate (GC_state s, GC_heap h, W32 desiredSize, W32 minSize) {
	W32 backoff;

	if (DEBUG_MEM)
		fprintf (stderr, "heapCreate  desired size = %s  min size = %s\n",
				uintToCommaString (desiredSize),
				uintToCommaString (minSize));
	assert (heapIsInit (h));
	if (desiredSize < minSize)
		desiredSize = minSize;
 	desiredSize = align (desiredSize, s->pageSize);
	assert (0 == h->size and NULL == h->start);
	backoff = (desiredSize - minSize) / 20;
	if (0 == backoff)
		backoff = 1; /* enough to terminate the loop below */
	backoff = align (backoff, s->pageSize);
	/* mmap toggling back and forth between high and low addresses to
         * decrease the chance of virtual memory fragmentation causing an mmap
	 * to fail.  This is important for large heaps.
	 */
	for (h->size = desiredSize; h->size >= minSize; h->size -= backoff) {
		static int direction = 1;
		int i;

		assert (isAligned (h->size, s->pageSize));
		for (i = 0; i < 32; i++) {
			unsigned long address;

			address = i * 0x08000000ul;
			if (direction)
				address = 0xf8000000ul - address;
			h->start = GC_mmapAnon ((void*)address, h->size);
			if ((void*)-1 == h->start)
				h->start = (void*)NULL;
			unless ((void*)NULL == h->start) {
				direction = (0 == direction);
				if (h->size > s->maxHeapSizeSeen)
					s->maxHeapSizeSeen = h->size;
				if (DEBUG or s->messages)
					fprintf (stderr, "Created heap of size %s at 0x%08x.\n",
							uintToCommaString (h->size),
							(uint)h->start);
				assert (h->size >= minSize);
				return TRUE;
			}
		}
		if (s->messages)
			fprintf(stderr, "[Requested %luM cannot be satisfied, backing off by %luM (min size = %luM).\n",
				meg (h->size), meg (backoff), meg (minSize));
	}
	h->size = 0;
	return FALSE;
}

static inline uint objectSize (GC_state s, pointer p) {
	Bool hasIdentity;
	uint headerBytes, objectBytes;
       	word header;
	uint tag, numPointers, numNonPointers;

	header = GC_getHeader (p);
	SPLIT_HEADER();
	if (NORMAL_TAG == tag) { /* Fixed size object. */
		headerBytes = GC_NORMAL_HEADER_SIZE;
		objectBytes = toBytes (numPointers + numNonPointers);
	} else if (ARRAY_TAG == tag) {
		headerBytes = GC_ARRAY_HEADER_SIZE;
		objectBytes = arrayNumBytes (s, p, numPointers, numNonPointers);
	} else if (WEAK_TAG == tag) {
		headerBytes = GC_NORMAL_HEADER_SIZE;
		objectBytes = sizeof (struct GC_weak);
	} else { /* Stack. */
		assert (STACK_TAG == tag);
		headerBytes = STACK_HEADER_SIZE;
		objectBytes = sizeof (struct GC_stack) + ((GC_stack)p)->reserved;
	}
	return headerBytes + objectBytes;
}

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

/* forward (s, pp) forwards the object pointed to by *pp and updates *pp to 
 * point to the new object. 
 * It also updates the crossMap.
 */
static inline void forward (GC_state s, pointer *pp) {
	pointer p;
	word header;
	word tag;

	if (DEBUG_DETAILED)
		fprintf (stderr, "forward  pp = 0x%x  *pp = 0x%x\n", (uint)pp, *(uint*)pp);
	assert (isInFromSpace (s, *pp));
	p = *pp;
	header = GC_getHeader (p);
	if (DEBUG_DETAILED and FORWARDED == header)
		fprintf (stderr, "already FORWARDED\n");
	if (header != FORWARDED) { /* forward the object */
		Bool hasIdentity;
		uint headerBytes, objectBytes, size, skip;
		uint numPointers, numNonPointers;

		/* Compute the space taken by the header and object body. */
		SPLIT_HEADER();
		if (NORMAL_TAG == tag) { /* Fixed size object. */
			headerBytes = GC_NORMAL_HEADER_SIZE;
			objectBytes = toBytes (numPointers + numNonPointers);
			skip = 0;
		} else if (ARRAY_TAG == tag) {
			headerBytes = GC_ARRAY_HEADER_SIZE;
			objectBytes = arrayNumBytes (s, p, numPointers,
							numNonPointers);
			skip = 0;
		} else if (WEAK_TAG == tag) {
			headerBytes = GC_NORMAL_HEADER_SIZE;
			objectBytes = sizeof (struct GC_weak);
			skip = 0;
		} else { /* Stack. */
			GC_stack stack;

			assert (STACK_TAG == tag);
			headerBytes = STACK_HEADER_SIZE;
			stack = (GC_stack)p;

			if (s->currentThread->stack == stack) {
				/* Shrink stacks that don't use a lot 
				 * of their reserved space;
				 * but don't violate the stack invariant.
				 */
				if (stack->used <= stack->reserved / 4) {
					uint new = stackReserved (s, max (stack->reserved / 2,
										stackNeedsReserved (s, stack)));
					/* It's possible that new > stack->reserved if
					 * the stack invariant is violated. In that case, 
					 * we want to leave the stack alone, because some 
					 * other part of the gc will grow the stack.  We 
					 * cannot do any growing here because we may run 
					 * out of to space.
					 */
					if (new <= stack->reserved) {
						stack->reserved = new;
						if (DEBUG_STACKS)
							fprintf (stderr, "Shrinking stack to size %s.\n",
									uintToCommaString (stack->reserved));
					}
				}
			} else {
				/* Shrink heap stacks.
				 */
				stack->reserved = stackReserved (s, max(s->threadShrinkRatio * stack->reserved, 
									stack->used));
				if (DEBUG_STACKS)
					fprintf (stderr, "Shrinking stack to size %s.\n",
							uintToCommaString (stack->reserved));
			}
			objectBytes = sizeof (struct GC_stack) + stack->used;
			skip = stack->reserved - stack->used;
		}
		size = headerBytes + objectBytes;
		assert (s->back + size + skip <= s->toLimit);
  		/* Copy the object. */
		copy (p - headerBytes, s->back, size);
		/* If the object has a valid weak pointer, link it into the weaks
		 * for update after the copying GC is done.
		 */
		if (WEAK_TAG == tag and 1 == numPointers) {
			GC_weak w;

			w = (GC_weak)(s->back + GC_NORMAL_HEADER_SIZE);
			if (DEBUG_WEAK)
				fprintf (stderr, "forwarding weak 0x%08x ",
						(uint)w);
			if (GC_isPointer (w->object)
				and (not s->amInMinorGC
					or isInNursery (s, w->object))) {
				if (DEBUG_WEAK)
					fprintf (stderr, "linking\n");
				w->link = s->weaks;
				s->weaks = w;
			} else {
				if (DEBUG_WEAK)
					fprintf (stderr, "not linking\n");
			}
		}
 		/* Store the forwarding pointer in the old object. */
		*(word*)(p - WORD_SIZE) = FORWARDED;
		*(pointer*)p = s->back + headerBytes;
		/* Update the back of the queue. */
		s->back += size + skip;
		assert (isAligned ((uint)s->back + GC_NORMAL_HEADER_SIZE,
					s->alignment));
	}
	*pp = *(pointer*)p;
	assert (isInToSpace (s, *pp));
}

static void updateWeaks (GC_state s) {
	GC_weak w;

	for (w = s->weaks; w != NULL; w = w->link) {
		assert ((pointer)BOGUS_POINTER != w->object);

		if (DEBUG_WEAK)
			fprintf (stderr, "updateWeaks  w = 0x%08x  ", (uint)w);
		if (FORWARDED == GC_getHeader ((pointer)w->object)) {
			if (DEBUG_WEAK)
				fprintf (stderr, "forwarded from 0x%08x to 0x%08x\n",
						(uint)w->object,
						(uint)*(pointer*)w->object);
			w->object = *(pointer*)w->object;
		} else {
			if (DEBUG_WEAK)
				fprintf (stderr, "cleared\n");
			*(GC_getHeaderp((pointer)w)) = WEAK_GONE_HEADER;
			w->object = (pointer)BOGUS_POINTER;
		}
	}
	s->weaks = NULL;
}

static void swapSemis (GC_state s) {
	struct GC_heap h;

	h = s->heap2;
	s->heap2 = s->heap;
	s->heap = h;
	setCardMapForMutator (s);
}

static inline bool detailedGCTime (GC_state s) {
	return s->summary;
}

static void cheneyCopy (GC_state s) {
	struct rusage ru_start;
	pointer toStart;

	assert (s->heap2.size >= s->oldGenSize);
	if (detailedGCTime (s))
		startTiming (&ru_start);
	s->numCopyingGCs++;
	s->toSpace = s->heap2.start;
	s->toLimit = s->heap2.start + s->heap2.size;
 	if (DEBUG or s->messages) {
		fprintf (stderr, "Major copying GC.\n");
	 	fprintf (stderr, "fromSpace = 0x%08x of size %s\n", 
				(uint) s->heap.start,
				uintToCommaString (s->heap.size));
		fprintf (stderr, "toSpace = 0x%08x of size %s\n",
				(uint) s->heap2.start,
				uintToCommaString (s->heap2.size));
	}
	assert (s->heap2.start != (void*)NULL);
	/* The next assert ensures there is enough space for the copy to succeed.
	 * It does not assert (s->heap2.size >= s->heap.size) because that
         * is too strong.
	 */
	assert (s->heap2.size >= s->oldGenSize);
	toStart = alignFrontier (s, s->heap2.start);
	s->back = toStart;
	foreachGlobal (s, forward);
	foreachPointerInRange (s, toStart, &s->back, TRUE, forward);
	updateWeaks (s);
	s->oldGenSize = s->back - s->heap2.start;
	s->bytesCopied += s->oldGenSize;
	if (DEBUG)
		fprintf (stderr, "%s bytes live.\n", 
				uintToCommaString (s->oldGenSize));
	swapSemis (s);
	clearCrossMap (s);
	s->lastMajor = GC_COPYING;
	if (detailedGCTime (s))
		stopTiming (&ru_start, &s->ru_gcCopy);		
 	if (DEBUG or s->messages)
		fprintf (stderr, "Major copying GC done.\n");
}

/* ---------------------------------------------------------------- */
/*                     Minor copying collection                     */
/* ---------------------------------------------------------------- */

#if ASSERT

static inline pointer crossMapCardStart (GC_state s, pointer p) {
	/* The p - 1 is so that a pointer to the beginning of a card
	 * falls into the index for the previous crossMap entry.
	 */
	return (p == s->heap.start)
		? s->heap.start
		: (p - 1) - ((uint)(p - 1) % s->cardSize);
}

/* crossMapIsOK is a slower, but easier to understand, way of computing the
 * crossMap.  updateCrossMap (below) incrementally updates the crossMap, checking
 * only the part of the old generation that it hasn't seen before.  crossMapIsOK
 * simply walks through the entire old generation.  It is useful to check that
 * the incremental update is working correctly.
 */
static bool crossMapIsOK (GC_state s) {
	pointer back;
	uint cardIndex;
	pointer cardStart;
	pointer front;
	uint i;
	static uchar *m;

	if (DEBUG)
		fprintf (stderr, "crossMapIsOK ()\n");
	m = smmap (s->crossMapSize);
	memset (m, CROSS_MAP_EMPTY, s->crossMapSize);
	back = s->heap.start + s->oldGenSize;
	cardIndex = 0;
	front = alignFrontier (s, s->heap.start);
loopObjects:
	assert (front <= back);
	cardStart = crossMapCardStart (s, front);
	cardIndex = divCardSize (s, cardStart - s->heap.start);
	m[cardIndex] = (front - cardStart) / WORD_SIZE;
	if (front < back) {
		front += objectSize (s, toData (s, front));
		goto loopObjects;
	}
	for (i = 0; i < cardIndex; ++i)
		assert (m[i] == s->crossMap[i]);
	GC_release (m, s->crossMapSize);
	return TRUE;
}

#endif /* ASSERT */

static void updateCrossMap (GC_state s) {
	GC_heap h;
	pointer cardEnd;
	uint cardIndex;
	pointer cardStart;
	pointer next;
	pointer objectStart;
	pointer oldGenEnd;

	h = &(s->heap);
	if (s->crossMapValidSize == s->oldGenSize)
		goto done;
	oldGenEnd = h->start + s->oldGenSize;
	objectStart = h->start + s->crossMapValidSize;
	if (objectStart == h->start) {
		cardIndex = 0;
		objectStart = alignFrontier (s, objectStart);
	} else
		cardIndex = divCardSize (s, (uint)(objectStart - 1 - h->start));
	cardStart = h->start + cardNumToSize (s, cardIndex);
	cardEnd = cardStart + s->cardSize;
loopObjects:
	assert (objectStart < oldGenEnd);
	assert ((objectStart == h->start or cardStart < objectStart)
			and objectStart <= cardEnd);
	next = objectStart + objectSize (s, toData (s, objectStart));
	if (next > cardEnd) {
		/* We're about to move to a new card, so we are looking at the
		 * last object boundary in the current card.  Store it in the 
		 * crossMap.
		 */
		uint offset;

		offset = (objectStart - cardStart) / WORD_SIZE;
		assert (offset < CROSS_MAP_EMPTY);
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "crossMap[%u] = %u\n", 
					cardIndex, offset);
		s->crossMap[cardIndex] = offset;
		cardIndex = divCardSize (s, next - 1 - h->start);
		cardStart = h->start + cardNumToSize (s, cardIndex);
		cardEnd = cardStart + s->cardSize;
	}
	objectStart = next;
	if (objectStart < oldGenEnd) 
		goto loopObjects;
	assert (objectStart == oldGenEnd);
	s->crossMap[cardIndex] = (oldGenEnd - cardStart) / WORD_SIZE;
	s->crossMapValidSize = s->oldGenSize;
done:
	assert (s->crossMapValidSize == s->oldGenSize);
	assert (crossMapIsOK (s));
}

static inline void forwardIfInNursery (GC_state s, pointer *pp) {
	pointer p;

	p = *pp;
	if (p < s->nursery)
		return;
	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "intergenerational pointer from 0x%08x to 0x%08x\n",
			(uint)pp, *(uint*)pp);
	assert (s->nursery <= p and p < s->limitPlusSlop);
	forward (s, pp);
}


/* Walk through all the cards and forward all intergenerational pointers. */
static void forwardInterGenerationalPointers (GC_state s) {
	pointer cardMap;
	uint cardNum;
	pointer cardStart;
	uchar *crossMap;
	GC_heap h;
	uint numCards;
	pointer objectStart;
	pointer oldGenStart;
	pointer oldGenEnd;

	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "Forwarding inter-generational pointers.\n");
	updateCrossMap (s);
	h = &s->heap;
	/* Constants. */
	cardMap = s->cardMap;
	crossMap = s->crossMap;
	numCards = divCardSize (s, align (s->oldGenSize, s->cardSize));
	oldGenStart = s->heap.start;
	oldGenEnd = oldGenStart + s->oldGenSize;
	/* Loop variables*/
	objectStart = alignFrontier (s, s->heap.start);
	cardNum = 0;
	cardStart = oldGenStart;
checkAll:
	assert (cardNum <= numCards);
	assert (isAlignedFrontier (s, objectStart));
	if (cardNum == numCards)
		goto done;
checkCard:
	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "checking card %u  objectStart = 0x%08x  cardEnd = 0x%08x\n",
				cardNum, 
				(uint)objectStart,
				(uint)oldGenStart + cardNumToSize (s, cardNum + 1));
	assert (objectStart < oldGenStart + cardNumToSize (s, cardNum + 1));
	if (cardMap[cardNum]) {
		pointer cardEnd;
		pointer orig;
		uint size;

		s->markedCards++;
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "card %u is marked  objectStart = 0x%08x\n", 
					cardNum, (uint)objectStart);
		orig = objectStart;
skipObjects:
		assert (isAlignedFrontier (s, objectStart));
		size = objectSize (s, toData (s, objectStart));
		if (objectStart + size < cardStart) {
			objectStart += size;
			goto skipObjects;
		}
		s->minorBytesSkipped += objectStart - orig;
		cardEnd = cardStart + s->cardSize;
		if (oldGenEnd < cardEnd) 
			cardEnd = oldGenEnd;
		assert (objectStart < cardEnd);
		orig = objectStart;
		/* If we ever add Weak.set, then there could be intergenerational
		 * weak pointers, in which case we would need to link the weak
		 * objects into s->weaks.  But for now, since there is no 
		 * Weak.set, the foreachPointerInRange will do the right thing
		 * on weaks, since the weak pointer will never be into the 
		 * nursery.
		 */
		objectStart = 
			foreachPointerInRange (s, objectStart, &cardEnd, FALSE,
						forwardIfInNursery);
		s->minorBytesScanned += objectStart - orig;
		if (objectStart == oldGenEnd)
			goto done;
		cardNum = divCardSize (s, objectStart - oldGenStart);
		cardStart = oldGenStart + cardNumToSize (s, cardNum);
		goto checkCard;
	} else {
		unless (CROSS_MAP_EMPTY == crossMap[cardNum])
			objectStart = cardStart + crossMap[cardNum] * WORD_SIZE;
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "card %u is not marked  crossMap[%u] == %u  objectStart = 0x%08x\n", 
					cardNum,
					cardNum, 
					crossMap[cardNum] * WORD_SIZE,
					(uint)objectStart);
		cardNum++;
		cardStart += s->cardSize;
		goto checkAll;
	}
	assert (FALSE);
done:
	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "Forwarding inter-generational pointers done.\n");
}

static void minorGC (GC_state s) {
	W32 bytesAllocated;
	W32 bytesCopied;
	struct rusage ru_start;

	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "minorGC  nursery = 0x%08x  frontier = 0x%08x\n", 
				(uint)s->nursery,
				(uint)s->frontier);
	assert (invariant (s));
	bytesAllocated = s->frontier - s->nursery;
	if (bytesAllocated == 0)
		return;
 	s->bytesAllocated += bytesAllocated;
	if (not s->canMinor) {
		s->oldGenSize += bytesAllocated;
		bytesCopied = 0;
	} else {
		if (DEBUG_GENERATIONAL or s->messages)
			fprintf (stderr, "Minor GC.\n");
		if (detailedGCTime (s))
			startTiming (&ru_start);
		s->amInMinorGC = TRUE;
		s->toSpace = s->heap.start + s->oldGenSize;
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "toSpace = 0x%08x\n",
					(uint)s->toSpace);
		assert (isAlignedFrontier (s, s->toSpace));
		s->toLimit = s->toSpace + bytesAllocated;
		assert (invariant (s));
		s->numMinorGCs++;
		s->numMinorsSinceLastMajor++;
		s->back = s->toSpace;
		/* Forward all globals.  Would like to avoid doing this once all
	 	 * the globals have been assigned.
		 */
		foreachGlobal (s, forwardIfInNursery);
		forwardInterGenerationalPointers (s);
		foreachPointerInRange (s, s->toSpace, &s->back, TRUE,
					forwardIfInNursery);
		updateWeaks (s);
		bytesCopied = s->back - s->toSpace;
		s->bytesCopiedMinor += bytesCopied;
		s->oldGenSize += bytesCopied;
		s->amInMinorGC = FALSE;
		if (detailedGCTime (s))
			stopTiming (&ru_start, &s->ru_gcMinor);
		if (DEBUG_GENERATIONAL or s->messages)
			fprintf (stderr, "Minor GC done.  %s bytes copied.\n",
					uintToCommaString (bytesCopied));
	}
}

/* ---------------------------------------------------------------- */
/*                       Object hash consing                        */
/* ---------------------------------------------------------------- */

/* Hashing based on Introduction to Algorithms by Cormen Leiserson, and Rivest.
 * Section numbers in parens.
 * k is key to be hashed.
 * table is of size 2^p  (it must be a power of two)
 * Open addressing (12.4), meaning that we stick the entries directly in the 
 *   table and probe until we find what we want.
 * Multiplication method (12.3.2), meaning that we compute the hash by 
 *   multiplying by a magic number, chosen by Knuth, and take the high-order p
 *   bits of the low order 32 bits.
 * Double hashing (12.4), meaning that we use two hash functions, the first to
 *   decide where to start looking and a second to decide at what offset to
 *   probe.  The second hash must be relatively prime to the table size, which
 *   we ensure by making it odd and keeping the table size as a power of 2.
 */

static GC_ObjectHashTable newTable (GC_state s) {
	int i;
	uint maxElementsSize;
	pointer regionStart;
	pointer regionEnd;
	GC_ObjectHashTable t;

	NEW (GC_ObjectHashTable, t);
	// Try to use space in the heap for the elements.
	if (not (heapIsInit (&s->heap2))) {
		if (DEBUG_SHARE)
			fprintf (stderr, "using heap2\n");
		// We have all of heap2 available.  Use it.
		regionStart = s->heap2.start;
		regionEnd = s->heap2.start + s->heap2.size;
	} else if (s->amInGC or not s->canMinor) {
		if (DEBUG_SHARE)
			fprintf (stderr, "using end of heap\n");
		regionStart = s->frontier;
		regionEnd = s->limitPlusSlop;
	} else {
		if (DEBUG_SHARE)
			fprintf (stderr, "using minor space\n");
		// Use the space available for a minor GC.
		assert (s->canMinor);
		regionStart = s->heap.start + s->oldGenSize;
		regionEnd = s->nursery;
	}
	maxElementsSize = (regionEnd - regionStart) / sizeof (*(t->elements));
	if (DEBUG_SHARE)
		fprintf (stderr, "maxElementsSize = %u\n", maxElementsSize);
	t->elementsSize = 64;  // some small power of two
	t->log2ElementsSize = 6;  // and its log base 2
	if (maxElementsSize < t->elementsSize) {
		if (DEBUG_SHARE)
			fprintf (stderr, "too small -- using malloc\n");
		t->elementsIsInHeap = FALSE;
		ARRAY (struct GC_ObjectHashElement *, t->elements, t->elementsSize);
	} else {
		t->elementsIsInHeap = TRUE;
		t->elements = (struct GC_ObjectHashElement*)regionStart;
		// Find the largest power of two that fits.
		for (; t->elementsSize <= maxElementsSize; 
			t->elementsSize <<= 1, t->log2ElementsSize++)
			; // nothing
		t->elementsSize >>= 1;
		t->log2ElementsSize--;
		assert (t->elementsSize <= maxElementsSize);
		for (i = 0; i < t->elementsSize; ++i)
			t->elements[i].object = NULL;
	}
	t->numElements = 0;
	t->mayInsert = TRUE;
	if (DEBUG_SHARE) {
		fprintf (stderr, "elementsIsInHeap = %s\n", 
				boolToString (t->elementsIsInHeap));
		fprintf (stderr, "elementsSize = %u\n", t->elementsSize);
		fprintf (stderr, "0x%08x = newTable ()\n", (uint)t);
	}
	return t;
}

static void destroyTable (GC_ObjectHashTable t) {
	unless (t->elementsIsInHeap)
		free (t->elements);
	free (t);
}

static inline Pointer tableInsert 
	(GC_state s, GC_ObjectHashTable t, W32 hash, Pointer object, 
		Bool mightBeThere, Header header, W32 tag, Pointer max) {
	GC_ObjectHashElement e;
	Header header2;
	static Bool init = FALSE;
	static int maxNumProbes = 0;
	static W64 mult; // magic multiplier for hashing
	int numProbes;
	W32 probe;
	word *p;
	word *p2;
	W32 slot; // slot in hash table we are considering

	if (DEBUG_SHARE)
		fprintf (stderr, "tableInsert (%u, 0x%08x, %s, 0x%08x, 0x%08x)\n",
				(uint)hash, (uint)object, 
				boolToString (mightBeThere),
				(uint)header, (uint)max);
	if (! init) {
		init = TRUE;
		mult = floor (((sqrt (5.0) - 1.0) / 2.0)
				* (double)0x100000000llu);
	}
	slot = (W32)(mult * (W64)hash) >> (32 - t->log2ElementsSize);
	probe = (1 == slot % 2) ? slot : slot - 1;
	if (DEBUG_SHARE)
		fprintf (stderr, "probe = 0x%08x\n", (uint)probe);
	assert (1 == probe % 2);
	numProbes = 0;
look:
	if (DEBUG_SHARE)
		fprintf (stderr, "slot = 0x%08x\n", (uint)slot);
	assert (0 <= slot and slot < t->elementsSize);
	numProbes++;
	e = &t->elements[slot];
	if (NULL == e->object) {
		/* It's not in the table.  Add it. */
		unless (t->mayInsert) {
			if (DEBUG_SHARE)
				fprintf (stderr, "not inserting\n");
			return object;
		}
		e->hash = hash;
		e->object = object;
		t->numElements++;
		if (numProbes > maxNumProbes) {
			maxNumProbes = numProbes;
			if (DEBUG_SHARE)
				fprintf (stderr, "numProbes = %d\n", numProbes);
		}
		return object;
	}
	unless (hash == e->hash) {
lookNext:
		slot = (slot + probe) % t->elementsSize;
		goto look;
	}
	unless (mightBeThere)
		goto lookNext;
	if (DEBUG_SHARE)
		fprintf (stderr, "comparing 0x%08x to 0x%08x\n",
				(uint)object, (uint)e->object);
	/* Compare object to e->object. */
	unless (object == e->object) {
		header2 = GC_getHeader (e->object);
		unless (header == header2)
			goto lookNext;
		for (p = (word*)object, p2 = (word*)e->object; 
				p < (word*)max; 
				++p, ++p2)
			unless (*p == *p2)
				goto lookNext;
		if (ARRAY_TAG == tag
			and (GC_arrayNumElements (object)
				!= GC_arrayNumElements (e->object)))
			goto lookNext;
	}
	/* object is equal to e->object. */
	return e->object;
}

static void maybeGrowTable (GC_state s, GC_ObjectHashTable t) {	
	int i;
	GC_ObjectHashElement oldElement;
	struct GC_ObjectHashElement *oldElements;
	uint oldSize;
	uint newSize;

	if (not t->mayInsert or t->numElements * 2 <= t->elementsSize)
		return;
	oldElements = t->elements;
	oldSize = t->elementsSize;
	newSize = oldSize * 2;
	if (DEBUG_SHARE)
		fprintf (stderr, "trying to grow table to size %d\n", newSize);
	// Try to alocate the new table.
	ARRAY_UNSAFE (struct GC_ObjectHashElement *, t->elements, newSize);
	if (NULL == t->elements) {
		t->mayInsert = FALSE;
		t->elements = oldElements;
		if (DEBUG_SHARE)
			fprintf (stderr, "unable to grow table\n");
		return;
	}
	t->elementsSize = newSize;
	t->log2ElementsSize++;
	for (i = 0; i < oldSize; ++i) {
		oldElement = &oldElements[i];
		unless (NULL == oldElement->object)
			tableInsert (s, t, oldElement->hash, oldElement->object,
					FALSE, 0, 0, 0);
	}
	if (t->elementsIsInHeap)
		t->elementsIsInHeap = FALSE;
	else
		free (oldElements);
	if (DEBUG_SHARE)
		fprintf (stderr, "done growing table\n");
}

static Pointer hashCons (GC_state s, Pointer object, Bool countBytesHashConsed) {
	Bool hasIdentity;
	Word32 hash;
	Header header;
	pointer max;
	uint numNonPointers;
	uint numPointers;
	word *p;
	Pointer res;
	GC_ObjectHashTable t;
	uint tag;

	if (DEBUG_SHARE)
		fprintf (stderr, "hashCons (0x%08x)\n", (uint)object);
	t = s->objectHashTable;
	header = GC_getHeader (object);
	SPLIT_HEADER ();
	if (hasIdentity) {
		/* Don't hash cons. */
		res = object;
		goto done;
	}
	assert (ARRAY_TAG == tag or NORMAL_TAG == tag);
	max = object
		+ (ARRAY_TAG == tag
			? arrayNumBytes (s, object,
						numPointers, numNonPointers)
			: toBytes (numPointers + numNonPointers));
	// Compute the hash.
	hash = header;
	for (p = (word*)object; p < (word*)max; ++p)
		hash = hash * 31 + *p;
	/* Insert into table. */
       	res = tableInsert (s, t, hash, object, TRUE, header, tag, (Pointer)max);
	maybeGrowTable (s, t);
	if (countBytesHashConsed and res != object) {
		uint amount;

		amount = max - object;
		if (ARRAY_TAG == tag)
			amount += GC_ARRAY_HEADER_SIZE;
		else
			amount += GC_NORMAL_HEADER_SIZE;
		s->bytesHashConsed += amount;
	}
done:
	if (DEBUG_SHARE)
		fprintf (stderr, "0x%08x = hashCons (0x%08x)\n", 
				(uint)res, (uint)object);
	return res;
}

static inline void maybeSharePointer (GC_state s,
					Pointer *pp, 
					Bool shouldHashCons) {
	unless (shouldHashCons)
		return;
	if (DEBUG_SHARE)
		fprintf (stderr, "maybeSharePointer  pp = 0x%08x  *pp = 0x%08x\n",
				(uint)pp, (uint)*pp);
	*pp = hashCons (s, *pp, FALSE);	
}

/* ---------------------------------------------------------------- */
/*                       Depth-first Marking                        */
/* ---------------------------------------------------------------- */

static inline uint *arrayCounterp (pointer a) {
	return ((uint*)a - 3);
}

static inline uint arrayCounter (pointer a) {
	return *(arrayCounterp (a));
}

static inline bool isMarked (pointer p) {
	return MARK_MASK & GC_getHeader (p);
}

static bool modeEqMark (MarkMode m, pointer p) {
	return (MARK_MODE == m) ? isMarked (p): not isMarked (p);
}

/* mark (s, p, m) sets all the mark bits in the object graph pointed to by p. 
 * If m is MARK_MODE, it sets the bits to 1.
 * If m is UNMARK_MODE, it sets the bits to 0.
 *
 * It returns the total size in bytes of the objects marked.
 */
W32 mark (GC_state s, pointer root, MarkMode mode, Bool shouldHashCons) {
	uint arrayIndex;
	pointer cur;  /* The current object being marked. */
	GC_offsets frameOffsets;
	Bool hasIdentity;
	Header* headerp;
	Header header;
	uint index; /* The i'th pointer in the object (element) being marked. */
	GC_frameLayout *layout;
	Header mark; /* Used to set or clear the mark bit. */
	pointer next; /* The next object to mark. */
	Header *nextHeaderp;
	Header nextHeader;
	uint numNonPointers;
	uint numPointers;
	pointer prev; /* The previous object on the mark stack. */
	W32 size; /* Total number of bytes marked. */
	uint tag;
	pointer todo; /* A pointer to the pointer in cur to next. */
	pointer top; /* The top of the next stack frame to mark. */

	if (modeEqMark (mode, root))
		/* Object has already been marked. */
		return 0;
	mark = (MARK_MODE == mode) ? MARK_MASK : 0;
	size = 0;
	cur = root;
	prev = NULL;
	headerp = GC_getHeaderp (cur);
	header = *(Header*)headerp;
	goto mark;	
markNext:
	/* cur is the object that was being marked.
	 * prev is the mark stack.
	 * next is the unmarked object to be marked.
	 * nextHeaderp points to the header of next.
	 * nextHeader is the header of next.
	 * todo is a pointer to the pointer inside cur that points to next.
	 */
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "markNext  cur = 0x%08x  next = 0x%08x  prev = 0x%08x  todo = 0x%08x\n",
				(uint)cur, (uint)next, (uint)prev, (uint)todo);
	assert (not modeEqMark (mode, next));
	assert (nextHeaderp == GC_getHeaderp (next));
	assert (nextHeader == GC_getHeader (next));
	assert (*(pointer*) todo == next);
	headerp = nextHeaderp;
	header = nextHeader;
	*(pointer*)todo = prev;
	prev = cur;
	cur = next;
mark:
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "mark  cur = 0x%08x  prev = 0x%08x  mode = %s\n",
				(uint)cur, (uint)prev,
				(mode == MARK_MODE) ? "mark" : "unmark");
	/* cur is the object to mark. 
	 * prev is the mark stack.
	 * headerp points to the header of cur.
	 * header is the header of cur.
	 */
	assert (not modeEqMark (mode, cur));
	assert (header == GC_getHeader (cur));
	assert (headerp == GC_getHeaderp (cur));
	header ^= 0x80000000;
	/* Store the mark.  In the case of an object that contains a pointer to
	 * itself, it is essential that we store the marked header before marking
	 * the internal pointers (markInNormal below).  If we didn't, then we
	 * would see the object as unmarked and traverse it again.
         */
	*headerp = header;
	SPLIT_HEADER();
	if (NORMAL_TAG == tag) {
		if (0 == numPointers) {
			/* There is nothing to mark. */
			size += GC_NORMAL_HEADER_SIZE + toBytes (numNonPointers);
normalDone:
			if (shouldHashCons)
				cur = hashCons (s, cur, TRUE);
			goto ret;
		}
		todo = cur + toBytes (numNonPointers);
		size += todo + toBytes (numPointers) - (pointer)headerp;
		index = 0;
markInNormal:
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInNormal  index = %d\n", index);
		assert (index < numPointers);
		next = *(pointer*)todo;
		if (not GC_isPointer (next)) {
markNextInNormal:
			assert (index < numPointers);
       			index++;
			if (index == numPointers) {
				*headerp = header & ~COUNTER_MASK;
				goto normalDone;
			}
			todo += POINTER_SIZE;
			goto markInNormal;
		}
		nextHeaderp = GC_getHeaderp (next);
		nextHeader = *nextHeaderp;
		if (mark == (nextHeader & MARK_MASK)) {
			maybeSharePointer (s, (pointer*)todo, shouldHashCons);
			goto markNextInNormal;
		}
		*headerp = (header & ~COUNTER_MASK) |
				(index << COUNTER_SHIFT);
		goto markNext;
	} else if (WEAK_TAG == tag) {
		/* Store the marked header and don't follow any pointers. */
		goto ret;
	} else if (ARRAY_TAG == tag) {
		/* When marking arrays:
		 *   arrayIndex is the index of the element to mark.
		 *   cur is the pointer to the array.
		 *   index is the index of the pointer within the element
		 *     (i.e. the i'th pointer is at index i).
		 *   todo is the start of the element.
		 */
		size += GC_ARRAY_HEADER_SIZE
			+ arrayNumBytes (s, cur, numPointers, numNonPointers);
		if (0 == numPointers or 0 == GC_arrayNumElements (cur)) {
			/* There is nothing to mark. */
arrayDone:
			if (shouldHashCons)
				cur = hashCons (s, cur, TRUE);
			goto ret;
		}
		/* Begin marking first element. */
		arrayIndex = 0;
		todo = cur;
markArrayElt:
		assert (arrayIndex < GC_arrayNumElements (cur));
		index = 0;
		/* Skip to the first pointer. */
		todo += numNonPointers;
markInArray:
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInArray arrayIndex = %u index = %u\n",
					arrayIndex, index);
		assert (arrayIndex < GC_arrayNumElements (cur));
		assert (index < numPointers);
		assert (todo == arrayPointer (s, cur, arrayIndex, index));
		next = *(pointer*)todo;
		if (not (GC_isPointer (next))) {
markNextInArray:
			assert (arrayIndex < GC_arrayNumElements (cur));
			assert (index < numPointers);
			assert (todo == arrayPointer (s, cur, arrayIndex, index));
			todo += POINTER_SIZE;
			index++;
			if (index < numPointers)
				goto markInArray;
			arrayIndex++;
			if (arrayIndex < GC_arrayNumElements (cur))
				goto markArrayElt;
			/* Done.  Clear out the counters and return. */
			*arrayCounterp (cur) = 0;
			*headerp = header & ~COUNTER_MASK;
			goto arrayDone;
		}
		nextHeaderp = GC_getHeaderp (next);
		nextHeader = *nextHeaderp;
		if (mark == (nextHeader & MARK_MASK)) {
			maybeSharePointer (s, (pointer*)todo, shouldHashCons);
			goto markNextInArray;
		}
		/* Recur and mark next. */
		*arrayCounterp (cur) = arrayIndex;
		*headerp = (header & ~COUNTER_MASK) |
				(index << COUNTER_SHIFT);
		goto markNext;
	} else {
		assert (STACK_TAG == tag);
		size += stackBytes (s, ((GC_stack)cur)->reserved);
		top = stackTop (s, (GC_stack)cur);
		assert (((GC_stack)cur)->used <= ((GC_stack)cur)->reserved);
markInStack:
		/* Invariant: top points just past the return address of the
		 * frame to be marked.
		 */
		assert (stackBottom (s, (GC_stack)cur) <= top);
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInStack  top = %d\n",
					top - stackBottom (s, (GC_stack)cur));
					
		if (top == stackBottom (s, (GC_stack)(cur)))
			goto ret;
		index = 0;
		layout = getFrameLayout (s, *(word*) (top - WORD_SIZE));
		frameOffsets = layout->offsets;
		((GC_stack)cur)->markTop = top;
markInFrame:
		if (index == frameOffsets [0]) {
			top -= layout->numBytes;
			goto markInStack;
		}
		todo = top - layout->numBytes + frameOffsets [index + 1];
		next = *(pointer*)todo;
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, 
				"    offset %u  todo 0x%08x  next = 0x%08x\n", 
				frameOffsets [index + 1], 
				(uint)todo, (uint)next);
		if (not GC_isPointer (next)) {
			index++;
			goto markInFrame;
		}
		nextHeaderp = GC_getHeaderp (next);
		nextHeader = *nextHeaderp;
		if (mark == (nextHeader & MARK_MASK)) {
			index++;
			maybeSharePointer (s, (pointer*)todo, shouldHashCons);
			goto markInFrame;
		}
		((GC_stack)cur)->markIndex = index;		
		goto markNext;
	}
	assert (FALSE);
ret:
	/* Done marking cur, continue with prev.
	 * Need to set the pointer in the prev object that pointed to cur 
	 * to point back to prev, and restore prev.
 	 */
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "return  cur = 0x%08x  prev = 0x%08x\n",
				(uint)cur, (uint)prev);
	assert (modeEqMark (mode, cur));
	if (NULL == prev)
		return size;
	next = cur;
	cur = prev;
	headerp = GC_getHeaderp (cur);
	header = *headerp;
	SPLIT_HEADER();
	/* It's impossible to get a WEAK_TAG here, since we would never follow
	 * the weak object pointer.
	 */
	assert (WEAK_TAG != tag);
	if (NORMAL_TAG == tag) {
		todo = cur + toBytes (numNonPointers);
		index = (header & COUNTER_MASK) >> COUNTER_SHIFT;
		todo += index * POINTER_SIZE;
		prev = *(pointer*)todo;
		*(pointer*)todo = next;
		goto markNextInNormal;
	} else if (ARRAY_TAG == tag) {
		arrayIndex = arrayCounter (cur);
		todo = cur + arrayIndex * (numNonPointers 
						+ toBytes (numPointers));
		index = (header & COUNTER_MASK) >> COUNTER_SHIFT;
		todo += numNonPointers + index * POINTER_SIZE;
		prev = *(pointer*)todo;
		*(pointer*)todo = next;
		goto markNextInArray;
	} else {
		assert (STACK_TAG == tag);
		index = ((GC_stack)cur)->markIndex;
		top = ((GC_stack)cur)->markTop;
		layout = getFrameLayout (s, *(word*) (top - WORD_SIZE));
		frameOffsets = layout->offsets;
		todo = top - layout->numBytes + frameOffsets [index + 1];
		prev = *(pointer*)todo;
		*(pointer*)todo = next;
		index++;
		goto markInFrame;
	}
	assert (FALSE);
}

static void bytesHashConsedMessage (GC_state s, ullong total) {
	fprintf (stderr, "%s bytes hash consed (%.1f%%).\n",
		ullongToCommaString (s->bytesHashConsed),
		100.0 * ((double)s->bytesHashConsed / (double)total));
}

void GC_share (GC_state s, Pointer object) {
	W32 total;

	if (DEBUG_SHARE)
		fprintf (stderr, "GC_share 0x%08x\n", (uint)object);
	if (DEBUG_SHARE or s->messages)
		s->bytesHashConsed = 0;
	// Don't hash cons during the first round of marking.
	total = mark (s, object, MARK_MODE, FALSE);
	s->objectHashTable = newTable (s);
	// Hash cons during the second round of marking.
	mark (s, object, UNMARK_MODE, TRUE);
	destroyTable (s->objectHashTable);
	if (DEBUG_SHARE or s->messages)
		bytesHashConsedMessage (s, total);
}

/* ---------------------------------------------------------------- */
/*                 Jonkers Mark-compact Collection                  */
/* ---------------------------------------------------------------- */

static inline void markGlobalTrue (GC_state s, pointer *pp) {
	mark (s, *pp, MARK_MODE, TRUE);
}

static inline void markGlobalFalse (GC_state s, pointer *pp) {
	mark (s, *pp, MARK_MODE, FALSE);
}

static inline void unmarkGlobal (GC_state s, pointer *pp) {
       	mark (s, *pp, UNMARK_MODE, FALSE);
}

static inline void threadInternal (GC_state s, pointer *pp) {
	Header *headerp;

	if (FALSE)
		fprintf (stderr, "threadInternal pp = 0x%08x  *pp = 0x%08x  header = 0x%08x\n",
				(uint)pp, *(uint*)pp, (uint)GC_getHeader (*pp));
	headerp = GC_getHeaderp (*pp);
	*(Header*)pp = *headerp;
	*headerp = (Header)pp;
}

/* If p is weak, the object pointer was valid, and points to an unmarked object,
 * then clear the object pointer.
 */
static inline void maybeClearWeak (GC_state s, pointer p) {
	Bool hasIdentity;
	Header header;
	Header *headerp;
	uint numPointers;
	uint numNonPointers;
	uint tag;

	headerp = GC_getHeaderp (p);
	header = *headerp;
	SPLIT_HEADER();
	if (WEAK_TAG == tag and 1 == numPointers) { 
		Header h2;

		if (DEBUG_WEAK)
			fprintf (stderr, "maybeClearWeak (0x%08x)  header = 0x%08x\n",
					(uint)p, (uint)header);
		h2 = GC_getHeader (((GC_weak)p)->object);
		/* If it's unmarked not threaded, clear the weak pointer. */
		if (1 == ((MARK_MASK | 1) & h2)) {
			((GC_weak)p)->object = (pointer)BOGUS_POINTER;
			header = WEAK_GONE_HEADER | MARK_MASK;
			if (DEBUG_WEAK)
				fprintf (stderr, "cleared.  new header = 0x%08x\n",
						(uint)header);
			*headerp = header;
		}
	}
}

static void updateForwardPointers (GC_state s) {
	pointer back;
	pointer front;
	uint gap;
	pointer endOfLastMarked;
	Header header;
	Header *headerp;
	pointer p;
	uint size;

	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "Update forward pointers.\n");
	front = alignFrontier (s, s->heap.start);
	back = s->heap.start + s->oldGenSize;
	endOfLastMarked = front;
	gap = 0;
updateObject:
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "updateObject  front = 0x%08x  back = 0x%08x\n",
				(uint)front, (uint)back);
	if (front == back)
		goto done;
	headerp = (Header*)front;
	header = *headerp;
	if (0 == header) {
		/* We're looking at an array.  Move to the header. */
		p = front + 3 * WORD_SIZE;
		headerp = (Header*)(p - WORD_SIZE);
		header = *headerp;
	} else 
		p = front + WORD_SIZE;
	if (1 == (1 & header)) {
		/* It's a header */
		if (MARK_MASK & header) {
			/* It is marked, but has no forward pointers. 
			 * Thread internal pointers.
			 */
thread:
			maybeClearWeak (s, p);
			size = objectSize (s, p);
			if (DEBUG_MARK_COMPACT)
	       			fprintf (stderr, "threading 0x%08x of size %u\n", 
						(uint)p, size);
			if (front - endOfLastMarked >= 4 * WORD_SIZE) {
				/* Compress all of the unmarked into one string.
				 * We require 4 * WORD_SIZE space to be available
				 * because that is the smallest possible array.
				 * You cannot use 3 * WORD_SIZE because even
				 * zero-length arrays require an extra word for
				 * the forwarding pointer.  If you did use
				 * 3 * WORD_SIZE, updateBackwardPointersAndSlide
				 * would skip the extra word and be completely
				 * busted.
				 */
				if (DEBUG_MARK_COMPACT)
					fprintf (stderr, "compressing from 0x%08x to 0x%08x (length = %u)\n",
							(uint)endOfLastMarked,
							(uint)front,
							front - endOfLastMarked);
				*(uint*)endOfLastMarked = 0;
				*(uint*)(endOfLastMarked + WORD_SIZE) = 
					front - endOfLastMarked - 3 * WORD_SIZE;
				*(uint*)(endOfLastMarked + 2 * WORD_SIZE) =
					GC_objectHeader (STRING_TYPE_INDEX);
			}
			front += size;
			endOfLastMarked = front;
			foreachPointerInObject (s, p, FALSE, threadInternal);
			goto updateObject;
		} else {
			/* It's not marked. */
			size = objectSize (s, p);
			gap += size;
			front += size;
			goto updateObject;
		}
	} else {
		pointer new;

		assert (0 == (3 & header));
		/* It's a pointer.  This object must be live.  Fix all the
		 * forward pointers to it, store its header, then thread
                 * its internal pointers.
		 */
		new = p - gap;
		do {
			pointer cur;

			cur = (pointer)header;
			header = *(word*)cur;
			*(word*)cur = (word)new;
		} while (0 == (1 & header));
		*headerp = header;
		goto thread;
	}
	assert (FALSE);
done:
	return;
}

static void updateBackwardPointersAndSlide (GC_state s) {
	pointer back;
	pointer front;
	uint gap;
	Header header;
	pointer p;
	uint size;

	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "Update backward pointers and slide.\n");
	front = alignFrontier (s, s->heap.start);
	back = s->heap.start + s->oldGenSize;
	gap = 0;
updateObject:
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "updateObject  front = 0x%08x  back = 0x%08x\n",
				(uint)front, (uint)back);
	if (front == back)
		goto done;
	header = *(word*)front;
	if (0 == header) {
		/* We're looking at an array.  Move to the header. */
		p = front + 3 * WORD_SIZE;
		header = *(Header*)(p - WORD_SIZE);
	} else 
		p = front + WORD_SIZE;
	if (1 == (1 & header)) {
		/* It's a header */
		if (MARK_MASK & header) {
			/* It is marked, but has no backward pointers to it.
			 * Unmark it.
			 */
unmark:
			*GC_getHeaderp (p) = header & ~MARK_MASK;
			size = objectSize (s, p);
			if (DEBUG_MARK_COMPACT)
				fprintf (stderr, "unmarking 0x%08x of size %u\n", 
						(uint)p, size);
			/* slide */
			if (DEBUG_MARK_COMPACT)
				fprintf (stderr, "sliding 0x%08x down %u\n",
						(uint)front, gap);
			copy (front, front - gap, size);
			front += size;
			goto updateObject;
		} else {
			/* It's not marked. */
			size = objectSize (s, p);
			if (DEBUG_MARK_COMPACT)
				fprintf (stderr, "skipping 0x%08x of size %u\n",
						(uint)p, size);
			gap += size;
			front += size;
			goto updateObject;
		}
	} else {
		pointer new;

		/* It's a pointer.  This object must be live.  Fix all the
		 * backward pointers to it.  Then unmark it.
		 */
		new = p - gap;
		do {
			pointer cur;

			assert (0 == (3 & header));
			cur = (pointer)header;
			header = *(word*)cur;
			*(word*)cur = (word)new;
		} while (0 == (1 & header));
		/* The header will be stored by unmark. */
		goto unmark;
	}
	assert (FALSE);
done:
	s->oldGenSize = front - gap - s->heap.start;
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "bytesLive = %u\n", s->bytesLive);
	return;
}

static void markCompact (GC_state s) {
	struct rusage ru_start;

	if (DEBUG or s->messages)
		fprintf (stderr, "Major mark-compact GC.\n");
	if (detailedGCTime (s))
		startTiming (&ru_start);
	s->numMarkCompactGCs++;
	if (s->hashConsDuringGC) {
		s->bytesHashConsed = 0;
		s->numHashConsGCs++;
		s->objectHashTable = newTable (s);
	}
	foreachGlobal (s, s->hashConsDuringGC 
				? markGlobalTrue 
				: markGlobalFalse);
	if (s->hashConsDuringGC)
		destroyTable (s->objectHashTable);
	foreachGlobal (s, threadInternal);
	updateForwardPointers (s);
	updateBackwardPointersAndSlide (s);
	clearCrossMap (s);
	s->bytesMarkCompacted += s->oldGenSize;
	s->lastMajor = GC_MARK_COMPACT;
	if (detailedGCTime (s))
		stopTiming (&ru_start, &s->ru_gcMarkCompact);
	if (DEBUG or s->messages) {
		fprintf (stderr, "Major mark-compact GC done.\n");
		if (s->hashConsDuringGC)
			bytesHashConsedMessage 
				(s, s->bytesHashConsed + s->oldGenSize);
	}
}

/* ---------------------------------------------------------------- */
/*                          translateHeap                           */
/* ---------------------------------------------------------------- */

static void translatePointer (GC_state s, pointer *p) {
	if (s->translateUp)
		*p += s->translateDiff;
	else
		*p -= s->translateDiff;
}

/* Translate all pointers to the heap from within the stack and the heap for
 * a heap that has moved from from to to.
 */
static void translateHeap (GC_state s, pointer from, pointer to, uint size) {
	pointer limit;

	if (DEBUG or s->messages)
		fprintf (stderr, "Translating heap of size %s from 0x%08x to 0x%08x.\n",
				uintToCommaString (size),
				(uint)from, (uint)to);
	if (from == to)
		return;
	else if (to > from) {
		s->translateDiff = to - from;
		s->translateUp = TRUE;
	} else {
		s->translateDiff = from - to;
		s->translateUp = FALSE;
	}
	/* Translate globals and heap. */
	foreachGlobal (s, translatePointer);
	limit = to + size;
	foreachPointerInRange (s, alignFrontier (s, to), &limit, FALSE,
				translatePointer);
}

/* ---------------------------------------------------------------- */
/*                            heapRemap                             */
/* ---------------------------------------------------------------- */

bool heapRemap (GC_state s, GC_heap h, W32 desired, W32 minSize) {
	W32 backoff;
	W32 size;

#if not HAS_REMAP
	return FALSE;
#endif
	assert (minSize <= desired);
	assert (desired >= h->size);
	desired = align (desired, s->pageSize);
	backoff = (desired - minSize) / 20;
	if (0 == backoff)
		backoff = 1; /* enough to terminate the loop below */
	backoff = align (backoff, s->pageSize);
	for (size = desired; size >= minSize; size -= backoff) {
		pointer new;

		new = remap (h->start, h->size, size);
		unless ((void*)-1 == new) {
			h->start = new;
			h->size = size;
			if (h->size > s->maxHeapSizeSeen)
				s->maxHeapSizeSeen = h->size;
			assert (minSize <= h->size and h->size <= desired);
			return TRUE;
		}
	}
	return FALSE;
}

/* ---------------------------------------------------------------- */
/*                             heapGrow                             */
/* ---------------------------------------------------------------- */

static void growHeap (GC_state s, W32 desired, W32 minSize) {
	GC_heap h;
	struct GC_heap h2;
	pointer old;
	uint size;

	h = &s->heap;
	assert (desired >= h->size);
	if (DEBUG_RESIZING)
		fprintf (stderr, "Growing heap at 0x%08x of size %s to %s bytes.\n",
				(uint)h->start,
				uintToCommaString (h->size),
       				uintToCommaString (desired));
	old = s->heap.start;
	size = s->oldGenSize;
	assert (size <= h->size);
	if (heapRemap (s, h, desired, minSize))
		goto done;
	heapShrink (s, h, size);
	heapInit (&h2);
	/* Allocate a space of the desired size. */
	if (heapCreate (s, &h2, desired, minSize)) {
		pointer from;
		uint remaining;
		pointer to;

		from = old + size;
		to = h2.start + size;
		remaining = size;
copy:			
		assert (remaining == from - old 
				and from >= old and to >= h2.start);
		if (remaining < COPY_CHUNK_SIZE) {
			copy (old, h2.start, remaining);
		} else {
			remaining -= COPY_CHUNK_SIZE;
			from -= COPY_CHUNK_SIZE;
			to -= COPY_CHUNK_SIZE;
			copy (from, to, COPY_CHUNK_SIZE);
			heapShrink (s, h, remaining);
			goto copy;
		}
		heapRelease (s, h);
		*h = h2;
	} else {
		/* Write the heap to a file and try again. */
		int fd;
		FILE *stream;
		char template[80];
		char *tmpDefault;
		char *tmpDir;
		char *tmpVar;

#if (defined (__MSVCRT__))
		tmpVar = "TEMP";
		tmpDefault = "C:/WINNT/TEMP";
#else
		tmpVar = "TMPDIR";
		tmpDefault = "/tmp";
#endif
		tmpDir = getenv (tmpVar);
		strcpy (template, (NULL == tmpDir) ? tmpDefault : tmpDir);
		strcat (template, "/FromSpaceXXXXXX");
		fd = smkstemp (template);
		sclose (fd);
		if (s->messages)
			fprintf (stderr, "Paging from space to %s.\n", 
					template);
		stream = sfopen (template, "wb");
		sfwrite (old, 1, size, stream);
		sfclose (stream);
		heapRelease (s, h);
		if (heapCreate (s, h, desired, minSize)) {
			stream = sfopen (template, "rb");
			sfread (h->start, 1, size, stream);
			sfclose (stream);
			sunlink (template);
		} else {
			sunlink (template);
			if (s->messages)
				showMem ();
			die ("Out of memory.  Unable to allocate %s bytes.\n",
				uintToCommaString (minSize));
		}
	}
done:
	unless (old == s->heap.start) {
		translateHeap (s, old, s->heap.start, s->oldGenSize);
		setCardMapForMutator (s);
	}
}


/* ---------------------------------------------------------------- */
/*                     resizeCardMapAndCrossMap                     */
/* ---------------------------------------------------------------- */

static void resizeCardMapAndCrossMap (GC_state s) {
	if (s->mutatorMarksCards 
		and s->cardMapSize != 
			align (divCardSize (s, s->heap.size), s->pageSize)) {
		pointer oldCardMap;
		uchar *oldCrossMap;
		uint oldCardMapSize;
		uint oldCrossMapSize;

		oldCardMap = s->cardMap;
		oldCardMapSize = s->cardMapSize;
		oldCrossMap = s->crossMap;
		oldCrossMapSize = s->crossMapSize;
		createCardMapAndCrossMap (s);
		copy ((pointer)oldCrossMap, (pointer)s->crossMap,
			min (s->crossMapSize, oldCrossMapSize));
		if (DEBUG_MEM)
			fprintf (stderr, "Releasing card/cross map.\n");
		GC_release (oldCardMap, oldCardMapSize + oldCrossMapSize);
	}
}

/* ---------------------------------------------------------------- */
/*                            resizeHeap                            */
/* ---------------------------------------------------------------- */
/* Resize from space and to space, guaranteeing that at least 'need' bytes are
 * available in from space.
 */
static void resizeHeap (GC_state s, W64 need) {
	W32 desired;

	if (DEBUG_RESIZING)
		fprintf (stderr, "resizeHeap  need = %s fromSize = %s\n",
				ullongToCommaString (need), 
				uintToCommaString (s->heap.size));
	desired = heapDesiredSize (s, need, s->heap.size);
	assert (need <= desired);
	if (desired <= s->heap.size)
		heapShrink (s, &s->heap, desired);
	else {
		heapRelease (s, &s->heap2);
		growHeap (s, desired, need);
	}
	resizeCardMapAndCrossMap (s);
	assert (s->heap.size >= need);
}

/* Guarantee that heap2 is either the same size as heap or is unmapped. */
static void resizeHeap2 (GC_state s) {
	uint size;
	uint size2;

	size = s->heap.size;
	size2 = s->heap2.size;
	if (DEBUG_RESIZING)
		fprintf (stderr, "resizeHeap2\n");
	if (0 == size2)
		return;
	if (2 * size > s->ram)
		/* Holding on to heap2 might cause paging.  So don't. */
		heapRelease (s, &s->heap2);
	else if (size2 < size) {
		unless (heapRemap (s, &s->heap2, size, size))
			heapRelease (s, &s->heap2);
        } else if (size2 > size)
		heapShrink (s, &s->heap2, size);
	assert (0 == s->heap2.size or s->heap.size == s->heap2.size);
}

static inline uint growStackSize (GC_state s) {
	return max (2 * s->currentThread->stack->reserved, 
			stackNeedsReserved (s, s->currentThread->stack));
}

static void growStack (GC_state s) {
	uint size;
	GC_stack stack;

	size = growStackSize (s);
	if (DEBUG_STACKS or s->messages)
		fprintf (stderr, "Growing stack to size %s.\n",
				uintToCommaString (stackBytes (s, size)));
	assert (hasBytesFree (s, stackBytes (s, size), 0));
	stack = newStack (s, size, TRUE);
	stackCopy (s, s->currentThread->stack, stack);
	s->currentThread->stack = stack;
	markCard (s, (pointer)s->currentThread);
}

/* ---------------------------------------------------------------- */
/*                        Garbage Collection                        */
/* ---------------------------------------------------------------- */

static bool heapAllocateSecondSemi (GC_state s, W32 size) {
	if ((s->fixedHeap > 0 and s->heap.size + size > s->fixedHeap)
		or (s->maxHeap > 0 and s->heap.size + size > s->maxHeap))
		return FALSE;
	return heapCreate (s, &s->heap2, size, s->oldGenSize);
}

static void majorGC (GC_state s, W32 bytesRequested, bool mayResize) {
	s->numMinorsSinceLastMajor = 0;
	if (0 < (s->numCopyingGCs + s->numMarkCompactGCs)
		and ((float)s->numHashConsGCs 
			/ (float)(s->numCopyingGCs + s->numMarkCompactGCs)
			< s->hashConsFrequency))
		s->hashConsDuringGC = TRUE;
        if ((not FORCE_MARK_COMPACT)
		and not s->hashConsDuringGC // only markCompact can hash cons
 		and s->heap.size < s->ram
		and (not heapIsInit (&s->heap2)
			or heapAllocateSecondSemi (s, heapDesiredSize (s, (W64)s->bytesLive + bytesRequested, 0))))
		cheneyCopy (s);
	else
		markCompact (s);
	s->hashConsDuringGC = FALSE;
	s->bytesLive = s->oldGenSize;
	if (s->bytesLive > s->maxBytesLive)
		s->maxBytesLive = s->bytesLive;
	/* Notice that the s->bytesLive below is different than the s->bytesLive
	 * used as an argument to heapAllocateSecondSemi above.  Above, it was 
         * an estimate.  Here, it is exactly how much was live after the GC.
	 */
	if (mayResize)
		resizeHeap (s, (W64)s->bytesLive + bytesRequested);
	resizeHeap2 (s);
	assert (s->oldGenSize + bytesRequested <= s->heap.size);
}

static inline void enterGC (GC_state s) {
	if (s->profilingIsOn) {
		/* We don't need to profileEnter for count profiling because it
		 * has already bumped the counter.  If we did allow the bump,
		 * then the count would look like function(s) had run an extra
 		 * time.
		 */  
		if (s->profileStack and not (PROFILE_COUNT == s->profileKind))
			GC_profileEnter (s);
		s->amInGC = TRUE;
	}
}

static inline void leaveGC (GC_state s) {
	if (s->profilingIsOn) {
		if (s->profileStack and not (PROFILE_COUNT == s->profileKind))
			GC_profileLeave (s);
		s->amInGC = FALSE;
	}
}

static inline bool needGCTime (GC_state s) {
	return DEBUG or s->summary or s->messages or s->rusageIsEnabled;
}

static void doGC (GC_state s, 
			W32 oldGenBytesRequested,
			W32 nurseryBytesRequested, 
			bool forceMajor,
			bool mayResize) {
	uint gcTime;
	bool stackTopOk;
	W64 stackBytesRequested;
	struct rusage ru_start;
	W64 totalBytesRequested;
	
	enterGC (s);
	if (DEBUG or s->messages)
		fprintf (stderr, "Starting gc.  Request %s nursery bytes and %s old gen bytes.\n",
				uintToCommaString (nurseryBytesRequested),
				uintToCommaString (oldGenBytesRequested));
	assert (invariant (s));
	if (needGCTime (s))
		startTiming (&ru_start);
	minorGC (s);
	stackTopOk = mutatorStackInvariant (s);
	stackBytesRequested = 
		stackTopOk ? 0 : stackBytes (s, growStackSize (s));
	totalBytesRequested = 
		(W64)oldGenBytesRequested 
		+ stackBytesRequested
		+ nurseryBytesRequested;
	if (forceMajor 
		or totalBytesRequested > s->heap.size - s->oldGenSize)
		majorGC (s, totalBytesRequested, mayResize);
	setNursery (s, oldGenBytesRequested + stackBytesRequested,
			nurseryBytesRequested);
	assert (hasBytesFree (s, oldGenBytesRequested + stackBytesRequested,
					nurseryBytesRequested));
	unless (stackTopOk)
		growStack (s);
	setStack (s);
	if (needGCTime (s)) {
		gcTime = stopTiming (&ru_start, &s->ru_gc);
		s->maxPause = max (s->maxPause, gcTime);
	} else
		gcTime = 0;  /* Assign gcTime to quell gcc warning. */
	if (DEBUG or s->messages) {
		fprintf (stderr, "Finished gc.\n");
		fprintf (stderr, "time: %s ms\n", intToCommaString (gcTime));
		fprintf (stderr, "old gen size: %s bytes (%.1f%%)\n", 
				intToCommaString (s->oldGenSize),
				100.0 * ((double) s->oldGenSize) 
					/ s->heap.size);
	}
	/* Send a GC signal. */
	if (s->handleGCSignal and s->signalHandler != BOGUS_THREAD) {
		if (DEBUG_SIGNALS)
			fprintf (stderr, "GC Signal pending.\n");
		s->gcSignalIsPending = TRUE;
		unless (s->inSignalHandler) 
			s->signalIsPending = TRUE;
	}
	if (DEBUG) 
		GC_display (s, stderr);
	assert (hasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
	assert (invariant (s));
	leaveGC (s);
}

static inline void ensureMutatorInvariant (GC_state s, bool force) {
	if (force
		or not (mutatorFrontierInvariant(s))
		or not (mutatorStackInvariant(s))) {
		/* This GC will grow the stack, if necessary. */
		doGC (s, 0, s->currentThread->bytesNeeded, force, TRUE);
	}
	assert (mutatorFrontierInvariant(s));
	assert (mutatorStackInvariant(s));
}

/* ensureFree (s, b) ensures that upon return
 *      b <= s->limitPlusSlop - s->frontier
 */
static inline void ensureFree (GC_state s, uint b) {
	assert (s->frontier <= s->limitPlusSlop);
	if (b > s->limitPlusSlop - s->frontier)
		doGC (s, 0, b, FALSE, TRUE);
	assert (b <= s->limitPlusSlop - s->frontier);
}

static void switchToThread (GC_state s, GC_thread t) {
	if (DEBUG_THREADS)
		fprintf (stderr, "switchToThread (0x%08x)  used = %u  reserved = %u\n", 
				(uint)t, t->stack->used, t->stack->reserved);
	s->currentThread = t;
	setStack (s);
}

/* GC_startHandler does not do an enter()/leave(), even though it is exported.
 * The basis library uses it via _import, not _prim, and so does not treat it
 * as a runtime call -- so the invariant in enter would fail miserably.  It is
 * OK because GC_startHandler must be called from within a critical section.
 *
 * Don't make it inline, because it is also called in basis/Thread.c, and when
 * compiling with COMPILE_FAST, they may appear out of order.
 */
void GC_startHandler (GC_state s) {
	/* Switch to the signal handler thread. */
	if (DEBUG_SIGNALS) {
		fprintf (stderr, "switching to signal handler\n");
		GC_display (s, stderr);
	}
	assert (s->canHandle == 1);
	assert (s->signalIsPending);
	s->signalIsPending = FALSE;
	s->inSignalHandler = TRUE;
	s->savedThread = s->currentThread;
	/* Set s->canHandle to 2 when switching to the signal handler thread;
	 * leaving the runtime will decrement s->canHandle to 1,
         * the signal handler will then run atomically and will finish by
         * switching to the thread to continue with, which will decrement
	 * s->canHandle to 0.
 	 */
	s->canHandle = 2;
}

static inline void maybeSwitchToHandler (GC_state s) {
	if (s->canHandle == 1 and s->signalIsPending) {
		GC_startHandler (s);
		switchToThread (s, s->signalHandler);
	}
}

void GC_switchToThread (GC_state s, GC_thread t, uint ensureBytesFree) {
	if (DEBUG_THREADS)
		fprintf (stderr, "GC_switchToThread (0x%08x, %u)\n", (uint)t, ensureBytesFree);
	if (FALSE) {
		/* This branch is slower than the else branch, especially 
		 * when debugging is turned on, because it does an invariant
		 * check on every thread switch.
		 * So, we'll stick with the else branch for now.
		 */
	 	enter (s);
		s->currentThread->bytesNeeded = ensureBytesFree;
		switchToThread (s, t);
		s->canHandle--;
		maybeSwitchToHandler (s);
		ensureMutatorInvariant (s, FALSE);
		assert (mutatorFrontierInvariant(s));
		assert (mutatorStackInvariant(s));
	 	leave (s);
	} else {
		/* BEGIN: enter(s); */
		s->currentThread->stack->used = currentStackUsed (s);
		s->currentThread->exnStack = s->exnStack;
		atomicBegin (s);
		/* END: enter(s); */
		s->currentThread->bytesNeeded = ensureBytesFree;
		switchToThread (s, t);
		s->canHandle--;
		maybeSwitchToHandler (s);
		/* BEGIN: ensureMutatorInvariant */
		if (not (mutatorFrontierInvariant(s))
			or not (mutatorStackInvariant(s))) {
			/* This GC will grow the stack, if necessary. */
			doGC (s, 0, s->currentThread->bytesNeeded, FALSE, TRUE);
		} 
		/* END: ensureMutatorInvariant */
       		/* BEGIN: leave(s); */
	       	atomicEnd (s);
		/* END: leave(s); */
	}
	assert (mutatorFrontierInvariant(s));
	assert (mutatorStackInvariant(s));
}

void GC_gc (GC_state s, uint bytesRequested, bool force,
		string file, int line) {
	if (DEBUG or s->messages)
		fprintf (stderr, "%s %d: GC_gc\n", file, line);
	enter (s);
	/* When the mutator requests zero bytes, it may actually need as much
	 * as LIMIT_SLOP.
	 */
	if (0 == bytesRequested)
		bytesRequested = LIMIT_SLOP;
	s->currentThread->bytesNeeded = bytesRequested;
 	maybeSwitchToHandler (s);
	ensureMutatorInvariant (s, force);
	assert (mutatorFrontierInvariant(s));
	assert (mutatorStackInvariant(s));
	leave (s);
}

/* ---------------------------------------------------------------- */
/*                         GC_arrayAllocate                         */
/* ---------------------------------------------------------------- */

pointer GC_arrayAllocate (GC_state s, W32 ensureBytesFree, W32 numElts, 
				W32 header) {
	W64 arraySize64;
	W32 arraySize;
	uint eltSize;
	W32 *frontier;
	Bool hasIdentity;
	Pointer last;
	uint numPointers;
	uint numNonPointers;
	pointer res;
	uint tag;

	SPLIT_HEADER();
	if (DEBUG)
		fprintf (stderr, "GC_arrayAllocate (0x%08x, %u, %u, 0x%08x)\n",
				(uint)s, (uint)ensureBytesFree, 
				(uint)numElts, (uint)header);
	eltSize = numPointers * POINTER_SIZE + numNonPointers;
	arraySize64 = 
		w64align ((W64)eltSize * (W64)numElts + GC_ARRAY_HEADER_SIZE,
				s->alignment);
	if (arraySize64 >= 0x100000000llu)
		die ("Out of memory: cannot allocate array with %s bytes.",
			ullongToCommaString (arraySize64));
	arraySize = (W32)arraySize64;
	if (arraySize < GC_ARRAY_HEADER_SIZE + WORD_SIZE)
		/* Create space for forwarding pointer. */
 		arraySize = GC_ARRAY_HEADER_SIZE + WORD_SIZE;
	if (DEBUG_ARRAY)
		fprintf (stderr, "array with %s elts of size %u and total size %s.  Ensure %s bytes free.\n",
			uintToCommaString (numElts), 
			(uint)eltSize, 
			uintToCommaString (arraySize),
			uintToCommaString (ensureBytesFree));
	if (arraySize >= s->oldGenArraySize) {
		enter (s);
		doGC (s,  arraySize, ensureBytesFree, FALSE, TRUE);
		leave (s);
		frontier = (W32*)(s->heap.start + s->oldGenSize);
		last = (pointer)frontier + arraySize;
		s->oldGenSize += arraySize;
		s->bytesAllocated += arraySize;
	} else {
		W32 require;

		require = arraySize + ensureBytesFree;
		if (require > s->limitPlusSlop - s->frontier) {
			enter (s);
			doGC (s, 0, require, FALSE, TRUE);
			leave (s);
		}
		frontier = (W32*)s->frontier;
		last = (pointer)frontier + arraySize;
		assert (isAlignedFrontier (s, last));
		s->frontier = last;
	}
	*frontier++ = 0; /* counter word */
	*frontier++ = numElts;
	*frontier++ = header;
	res = (pointer)frontier;
	/* Initialize all pointers with BOGUS_POINTER. */
	if (1 <= numPointers and 0 < numElts) {
		pointer p;

		if (0 == numNonPointers)
			for (p = (pointer)frontier; 
				p < last; 
				p += POINTER_SIZE)
				*(Pointer*)p = (Pointer)BOGUS_POINTER;
		else
			for (p = (Pointer)frontier; p < last; ) {
				pointer next;

				p += numNonPointers;
				next = p + numPointers * POINTER_SIZE;
				assert (next <= last);
				while (p < next) {
					*(Pointer*)p = (Pointer)BOGUS_POINTER;
					p += POINTER_SIZE;
				}	
			}
	}
	GC_profileAllocInc (s, arraySize);
	if (DEBUG_ARRAY) {
		fprintf (stderr, "GC_arrayAllocate done.  res = 0x%x  frontier = 0x%x\n",
				(uint)res, (uint)s->frontier);
		GC_display (s, stderr);
	}
	assert (ensureBytesFree <= s->limitPlusSlop - s->frontier);
	/* Unfortunately, the invariant isn't quite true here, because unless we
 	 * did the GC, we never set s->currentThread->stack->used to reflect
	 * what the mutator did with stackTop.
 	 */
	return res;
}	

/* ---------------------------------------------------------------- */
/*                             Threads                              */
/* ---------------------------------------------------------------- */

static inline uint threadBytes (GC_state s) {
	uint res;

	res = GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread);
	/* The following assert depends on struct GC_thread being the right
 	 * size.  Right now, it happens that res = 16, which is aligned mod 4
	 * and mod 8, which is all that we need.  If the struct every changes
	 * (possible) or we need more alignment (doubtful), we may need to put
	 * some padding at the beginning.
	 */
	assert (isAligned (res, s->alignment));
	return res;
}

static GC_thread newThreadOfSize (GC_state s, uint stackSize) {
	GC_stack stack;
	GC_thread t;

	ensureFree (s, stackBytes (s, stackSize) + threadBytes (s));
	stack = newStack (s, stackSize, FALSE);
	t = (GC_thread) object (s, THREAD_HEADER, threadBytes (s), FALSE, FALSE);
	t->bytesNeeded = 0;
	t->exnStack = BOGUS_EXN_STACK;
	t->stack = stack;
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%x = newThreadOfSize (%u)\n",
				(uint)t, stackSize);;
	return t;
}

static GC_thread copyThread (GC_state s, GC_thread from, uint size) {
	GC_thread to;

	if (DEBUG_THREADS)
		fprintf (stderr, "copyThread (0x%08x)\n", (uint)from);
	/* newThreadOfSize may do a GC, which invalidates from.  
	 * Hence we need to stash from where the GC can find it.
	 */
	s->savedThread = from;
	to = newThreadOfSize (s, size);	
	from = s->savedThread;
	s->savedThread = BOGUS_THREAD;
	if (DEBUG_THREADS) {
		fprintf (stderr, "free space = %u\n",
				s->limitPlusSlop - s->frontier);
		fprintf (stderr, "0x%08x = copyThread (0x%08x)\n", 
				(uint)to, (uint)from);
	}
	stackCopy (s, from->stack, to->stack);
	to->exnStack = from->exnStack;
	return to;
}

void GC_copyCurrentThread (GC_state s) {
	GC_thread res;
	GC_thread t;
	
	if (DEBUG_THREADS)
		fprintf (stderr, "GC_copyCurrentThread\n");
	enter (s);
	t = s->currentThread;
	res = copyThread (s, t, t->stack->used);
/* The following assert is no longer true, since alignment restrictions can force
 * the reserved to be slightly larger than the used.
 */
/*	assert (res->stack->reserved == res->stack->used); */
	assert (res->stack->reserved >= res->stack->used);
	leave (s);
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%08x = GC_copyCurrentThread\n", (uint)res);
	s->savedThread = res;
}

pointer GC_copyThread (GC_state s, pointer thread) {
	GC_thread res;
	GC_thread t;

	if (DEBUG_THREADS)
		fprintf (stderr, "GC_copyThread (0x%08x)\n", (uint)thread);
	enter (s);
	t = (GC_thread)thread;
/* The following assert is no longer true, since alignment restrictions can force
 * the reserved to be slightly larger than the used.
 */
/*	assert (t->stack->reserved == t->stack->used); */
	assert (t->stack->reserved >= t->stack->used);
	res = copyThread (s, t, t->stack->used);
/* The following assert is no longer true, since alignment restrictions can force
 * the reserved to be slightly larger than the used.
 */
/*	assert (res->stack->reserved == res->stack->used); */
	assert (res->stack->reserved >= res->stack->used);
	leave (s);
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%08x = GC_copyThread (0x%08x)\n", (uint)res, (uint)thread);
	return (pointer)res;
}

/* ---------------------------------------------------------------- */
/*                            Profiling                             */
/* ---------------------------------------------------------------- */

/* Apply f to the frame index of each frame in the current thread's stack. */
void GC_foreachStackFrame (GC_state s, void (*f) (GC_state s, uint i)) {
	pointer bottom;
	word index;
	GC_frameLayout *layout;
	word returnAddress;
	pointer top;

	if (DEBUG_PROFILE)
		fprintf (stderr, "walking stack");
	bottom = stackBottom (s, s->currentThread->stack);
	if (DEBUG_PROFILE)
		fprintf (stderr, "  bottom = 0x%08x  top = 0x%08x.\n",
				(uint)bottom, (uint)s->stackTop);
	for (top = s->stackTop; top > bottom; top -= layout->numBytes) {
		returnAddress = *(word*)(top - WORD_SIZE);
		index = getFrameIndex (s, returnAddress);
		if (DEBUG_PROFILE)
			fprintf (stderr, "top = 0x%08x  index = %u\n",
					(uint)top, index);
		unless (0 <= index and index < s->frameLayoutsSize)
			die ("top = 0x%08x  returnAddress = 0x%08x  index = %u\n",
					(uint)top, returnAddress, index);
		f (s, index);
		layout = &(s->frameLayouts[index]);
		assert (layout->numBytes > 0);
	}
	if (DEBUG_PROFILE)
		fprintf (stderr, "done walking stack\n");
}

static int numStackFrames;
static int *callStack;

static void addToCallStack (GC_state s, uint i) {
	if (DEBUG_CALL_STACK)
		fprintf (stderr, "addToCallStack (%u)\n", i);
	callStack[numStackFrames] = i;
	numStackFrames++;
}

void GC_callStack (GC_state s, Pointer p) {
	if (DEBUG_CALL_STACK)
		fprintf (stderr, "GC_callStack\n");
	numStackFrames = 0;
	callStack = (int*)p;
	GC_foreachStackFrame (s, addToCallStack);
}

uint * GC_frameIndexSourceSeq (GC_state s, int frameIndex) {
	uint *res;

	res = s->sourceSeqs[s->frameSources[frameIndex]];
	if (DEBUG_CALL_STACK)
		fprintf (stderr, "0x%08x = GC_frameIndexSourceSeq (%u)\n",
				(uint)res, frameIndex);
	return res;
}

static void bumpStackFrameCount (GC_state s, uint i) {
	numStackFrames++;
}

int GC_numStackFrames (GC_state s) {
	numStackFrames = 0;
	GC_foreachStackFrame (s, bumpStackFrameCount);
	if (DEBUG_CALL_STACK)
		fprintf (stderr, "%u = GC_numStackFrames\n", numStackFrames);
	return numStackFrames;
}

inline string GC_sourceName (GC_state s, uint i) {
	if (i < s->sourcesSize)
		return s->sourceNames[s->sources[i].nameIndex];
	else
		return s->sourceNames[i - s->sourcesSize];
}

static inline GC_profileStack profileStackInfo (GC_state s, uint i) {
	assert (s->profile != NULL);
	return &(s->profile->stack[i]);
}

static inline uint profileMaster (GC_state s, uint i) {
	return s->sources[i].nameIndex + s->sourcesSize;
}

static inline void removeFromStack (GC_state s, uint i) {
	GC_profile p;
	GC_profileStack ps;
	ullong totalInc;

	p = s->profile;
	ps = profileStackInfo (s, i);
	totalInc = p->total - ps->lastTotal;
	if (DEBUG_PROFILE)
		fprintf (stderr, "removing %s from stack  ticksInc = %llu  ticksInGCInc = %llu\n",
				GC_sourceName (s, i), totalInc,
				p->totalGC - ps->lastTotalGC);
	ps->ticks += totalInc;
	ps->ticksInGC += p->totalGC - ps->lastTotalGC;
}

static void setProfTimer (long usec) {
	struct itimerval iv;

	iv.it_interval.tv_sec = 0;
	iv.it_interval.tv_usec = usec;
	iv.it_value.tv_sec = 0;
	iv.it_value.tv_usec = usec;
	unless (0 == setitimer (ITIMER_PROF, &iv, NULL))
		die ("setProfTimer failed");
}

void GC_profileDone (GC_state s) {
	GC_profile p;
	uint sourceIndex;

	if (DEBUG_PROFILE) 
		fprintf (stderr, "GC_profileDone ()\n");
	assert (s->profilingIsOn);
	if (PROFILE_TIME == s->profileKind)
		setProfTimer (0);
	s->profilingIsOn = FALSE;
	p = s->profile;
	if (s->profileStack) {
		for (sourceIndex = 0; 
			sourceIndex < s->sourcesSize + s->sourceNamesSize;
			++sourceIndex) {
			if (p->stack[sourceIndex].numOccurrences > 0) {
				if (DEBUG_PROFILE)
					fprintf (stderr, "done leaving %s\n", 
							GC_sourceName (s, sourceIndex));
				removeFromStack (s, sourceIndex);
			}
		}
	}
}

static int profileDepth = 0;

static void profileIndent () {
	int i;

	for (i = 0; i < profileDepth; ++i)
		fprintf (stderr, " ");
}

static inline void profileEnterSource (GC_state s, uint i) {
	GC_profile p;
	GC_profileStack ps;

	p = s->profile;
	ps = profileStackInfo (s, i);
	if (0 == ps->numOccurrences) {
		ps->lastTotal = p->total;
		ps->lastTotalGC = p->totalGC;
	}
	ps->numOccurrences++;
}

static void profileEnter (GC_state s, uint sourceSeqIndex) {
	int i;
	GC_profile p;
	uint sourceIndex;
	uint *sourceSeq;

	if (DEBUG_PROFILE)
		fprintf (stderr, "profileEnter (%u)\n", sourceSeqIndex);
	assert (s->profileStack);
	assert (sourceSeqIndex < s->sourceSeqsSize);
	p = s->profile;
	sourceSeq = s->sourceSeqs[sourceSeqIndex];
	for (i = 1; i <= sourceSeq[0]; ++i) {
		sourceIndex = sourceSeq[i];
		if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
			profileIndent ();
			fprintf (stderr, "(entering %s\n", 
					GC_sourceName (s, sourceIndex));
			profileDepth++;
		}
		profileEnterSource (s, sourceIndex);
		profileEnterSource (s, profileMaster (s, sourceIndex));
	}
}

static void enterFrame (GC_state s, uint i) {
	profileEnter (s, s->frameSources[i]);
}

static inline void profileLeaveSource (GC_state s, uint i) {
	GC_profile p;
	GC_profileStack ps;

	if (DEBUG_PROFILE)
		fprintf (stderr, "profileLeaveSource (%u)\n", i);
	p = s->profile;
	ps = profileStackInfo (s, i);
	assert (ps->numOccurrences > 0);
	ps->numOccurrences--;
	if (0 == ps->numOccurrences)
		removeFromStack (s, i);
}

static void profileLeave (GC_state s, uint sourceSeqIndex) {
	int i;
	GC_profile p;
	uint sourceIndex;
	uint *sourceSeq;

	if (DEBUG_PROFILE)
		fprintf (stderr, "profileLeave (%u)\n", sourceSeqIndex);
	assert (s->profileStack);
	assert (sourceSeqIndex < s->sourceSeqsSize);
	p = s->profile;
	sourceSeq = s->sourceSeqs[sourceSeqIndex];
	for (i = sourceSeq[0]; i > 0; --i) {
		sourceIndex = sourceSeq[i];
		if (DEBUG_ENTER_LEAVE or DEBUG_PROFILE) {
			profileDepth--;
			profileIndent ();
			fprintf (stderr, "leaving %s)\n",
					GC_sourceName (s, sourceIndex));
		}
		profileLeaveSource (s, sourceIndex);
		profileLeaveSource (s, profileMaster (s, sourceIndex));
	}
}

static inline void profileInc (GC_state s, W32 amount, uint sourceSeqIndex) {
	uint *sourceSeq;
	uint topSourceIndex;

	if (DEBUG_PROFILE)
		fprintf (stderr, "profileInc (%u, %u)\n", 
				(uint)amount, sourceSeqIndex);
	assert (sourceSeqIndex < s->sourceSeqsSize);
	sourceSeq = s->sourceSeqs[sourceSeqIndex];
	topSourceIndex = sourceSeq[0] > 0
		? sourceSeq[sourceSeq[0]]
		: SOURCES_INDEX_UNKNOWN;
	if (DEBUG_PROFILE) {
		profileIndent ();
		fprintf (stderr, "bumping %s by %u\n",
				GC_sourceName (s, topSourceIndex), (uint)amount);
	}
	s->profile->countTop[topSourceIndex] += amount;
	s->profile->countTop[profileMaster (s, topSourceIndex)] += amount;
	if (s->profileStack)
		profileEnter (s, sourceSeqIndex);
	if (SOURCES_INDEX_GC == topSourceIndex)
		s->profile->totalGC += amount;
	else
		s->profile->total += amount;
	if (s->profileStack)
		profileLeave (s, sourceSeqIndex);
}

void GC_profileEnter (GC_state s) {
	profileEnter (s, topFrameSourceSeqIndex (s));
}

void GC_profileLeave (GC_state s) {
	profileLeave (s, topFrameSourceSeqIndex (s));
}

void GC_profileInc (GC_state s, W32 amount) {
	if (DEBUG_PROFILE)
		fprintf (stderr, "GC_profileInc (%u)\n", (uint)amount);
	profileInc (s, amount, 
			 s->amInGC
				? SOURCE_SEQ_GC 
				: topFrameSourceSeqIndex (s));
}

void GC_profileAllocInc (GC_state s, W32 amount) {
	if (s->profilingIsOn and (PROFILE_ALLOC == s->profileKind)) {
		if (DEBUG_PROFILE)
			fprintf (stderr, "GC_profileAllocInc (%u)\n", (uint)amount);
		GC_profileInc (s, amount);
	}
}

static void showProf (GC_state s) {
	int i;
	int j;

	fprintf (stdout, "0x%08x\n", s->magic);
	fprintf (stdout, "%u\n", s->sourceNamesSize);
	for (i = 0; i < s->sourceNamesSize; ++i)
		fprintf (stdout, "%s\n", s->sourceNames[i]);
	fprintf (stdout, "%u\n", s->sourcesSize);
	for (i = 0; i < s->sourcesSize; ++i)
		fprintf (stdout, "%u %u\n", 
				s->sources[i].nameIndex,
				s->sources[i].successorsIndex);
	fprintf (stdout, "%u\n", s->sourceSeqsSize);
	for (i = 0; i < s->sourceSeqsSize; ++i) {
		uint *sourceSeq;

		sourceSeq = s->sourceSeqs[i];
		for (j = 1; j <= sourceSeq[0]; ++j)
			fprintf (stdout, "%u ", sourceSeq[j]);
		fprintf (stdout, "\n");
	}
}

GC_profile GC_profileNew (GC_state s) {
	GC_profile p;
	uint size;

	NEW (GC_profile, p);
	p->total = 0;
	p->totalGC = 0;
	size = s->sourcesSize + s->sourceNamesSize;
	ARRAY (ullong*, p->countTop, size);
	if (s->profileStack)
		ARRAY (struct GC_profileStack *, p->stack, size);
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%08x = GC_profileNew ()\n", (uint)p);
	return p;
}

void GC_profileFree (GC_state s, GC_profile p) {
	free (p->countTop);
	if (s->profileStack)
		free (p->stack);
	free (p);
}

static void writeString (int fd, string s) {
	swrite (fd, s, strlen(s));
}

static void writeUint (int fd, uint u) {
	char buf[20];

	sprintf (buf, "%u", u);
	writeString (fd, buf);
}

static void writeUllong (int fd, ullong u) {
	char buf[20];

	sprintf (buf, "%llu", u);
	writeString (fd, buf);
}

static void writeWord (int fd, word w) {
	char buf[20];

	sprintf (buf, "0x%08x", w);
	writeString (fd, buf);
}

static inline void newline (int fd) {
	writeString (fd, "\n");
}

static void profileWriteCount (GC_state s, GC_profile p, int fd, uint i) {
	writeUllong (fd, p->countTop[i]);
	if (s->profileStack) {
		GC_profileStack ps;
	
		ps = &(p->stack[i]);
		writeString (fd, " ");
		writeUllong (fd, ps->ticks);
		writeString (fd, " ");
		writeUllong (fd, ps->ticksInGC);
	}
	newline (fd);
}

void GC_profileWrite (GC_state s, GC_profile p, int fd) {
	int i;
	string kind;

	if (DEBUG_PROFILE)
		fprintf (stderr, "GC_profileWrite\n");
	writeString (fd, "MLton prof\n");
	kind = "";
	switch (s->profileKind) {
	case PROFILE_ALLOC:
		kind = "alloc\n";
	break;
	case PROFILE_COUNT:
		kind = "count\n";
	break;
	case PROFILE_NONE:
		die ("impossible PROFILE_NONE");
	break;
	case PROFILE_TIME:
		kind = "time\n";
	break;
	}
	writeString (fd, kind);
	writeString (fd, s->profileStack 
				? "stack\n" : "current\n");
	writeWord (fd, s->magic);
	newline (fd);
	writeUllong (fd, p->total);
	writeString (fd, " ");
	writeUllong (fd, p->totalGC);
	newline (fd);
	writeUint (fd, s->sourcesSize);
	newline (fd);
	for (i = 0; i < s->sourcesSize; ++i)
		profileWriteCount (s, p, fd, i);
	writeUint (fd, s->sourceNamesSize);
	newline (fd);
	for (i = 0; i < s->sourceNamesSize; ++i)
		profileWriteCount (s, p, fd, i + s->sourcesSize);
}

#if not HAS_TIME_PROFILING

/* No time profiling on this platform.  There is a check in mlton/main/main.fun
 * to make sure that time profiling is never turned on.
 */
static void profileTimeInit (GC_state s) {
	die ("no time profiling");
}

#else

static GC_state catcherState;

void GC_handleSigProf (pointer pc) {
	uint frameIndex;
	GC_state s;
	uint sourceSeqIndex;

	s = catcherState;
	if (DEBUG_PROFILE)
		fprintf (stderr, "GC_handleSigProf (0x%08x)\n", (uint)pc);
	if (s->amInGC)
		sourceSeqIndex = SOURCE_SEQ_GC;
	else {
		frameIndex = topFrameIndex (s);
		if (s->frameLayouts[frameIndex].isC)
			sourceSeqIndex = s->frameSources[frameIndex];
		else {
			if (s->textStart <= pc and pc < s->textEnd)
				sourceSeqIndex = s->textSources [pc - s->textStart];
			else {
				if (DEBUG_PROFILE)
					fprintf (stderr, "pc out of bounds\n");
		       		sourceSeqIndex = SOURCE_SEQ_UNKNOWN;
			}
		}
	}
	profileInc (s, 1, sourceSeqIndex);
}

static int compareProfileLabels (const void *v1, const void *v2) {
	GC_profileLabel l1;
	GC_profileLabel l2;

	l1 = (GC_profileLabel)v1;
	l2 = (GC_profileLabel)v2;
	return (int)l1->label - (int)l2->label;
}

static void profileTimeInit (GC_state s) {
	int i;
	pointer p;
	struct sigaction sa;
	uint sourceSeqsIndex;

	s->profile = GC_profileNew (s);
	/* Sort sourceLabels by address. */
	qsort (s->sourceLabels, s->sourceLabelsSize, sizeof (*s->sourceLabels),
		compareProfileLabels);
	if (0 == s->sourceLabels[s->sourceLabelsSize - 1].label)
		die ("Max profile label is 0 -- something is wrong.");
	if (DEBUG_PROFILE)
		for (i = 0; i < s->sourceLabelsSize; ++i)
			fprintf (stderr, "0x%08x  %u\n",
					(uint)s->sourceLabels[i].label,
					s->sourceLabels[i].sourceSeqsIndex);
	if (ASSERT)
		for (i = 1; i < s->sourceLabelsSize; ++i)
			assert (s->sourceLabels[i-1].label
				<= s->sourceLabels[i].label);
	/* Initialize s->textSources. */
	s->textEnd = (pointer)(getTextEnd());
	s->textStart = (pointer)(getTextStart());
	if (ASSERT)
		for (i = 0; i < s->sourceLabelsSize; ++i) {
			pointer label;

			label = s->sourceLabels[i].label;
			assert (0 == label
				or (s->textStart <= label 
					and label < s->textEnd));
		}
	ARRAY (uint*, s->textSources, s->textEnd - s->textStart);
	p = s->textStart;
	sourceSeqsIndex = SOURCE_SEQ_UNKNOWN;
	for (i = 0; i < s->sourceLabelsSize; ++i) {
		for ( ; p < s->sourceLabels[i].label; ++p)
			s->textSources[p - s->textStart] = sourceSeqsIndex;
		sourceSeqsIndex = s->sourceLabels[i].sourceSeqsIndex;
	}
	for ( ; p < s->textEnd; ++p)
		s->textSources[p - s->textStart] = sourceSeqsIndex;
 	/*
	 * Install catcher, which handles SIGPROF and calls MLton_Profile_inc.
	 * 
	 * One thing I should point out that I discovered the hard way: If
	 * the call to sigaction does NOT specify the SA_ONSTACK flag, then
	 * even if you have called sigaltstack(), it will NOT switch stacks,
	 * so you will probably die.  Worse, if the call to sigaction DOES
	 * have SA_ONSTACK and you have NOT called sigaltstack(), it still
	 * switches stacks (to location 0) and you die of a SEGV.  Thus the
	 * sigaction() call MUST occur after the call to sigaltstack(), and
	 * in order to have profiling cover as much as possible, you want it
	 * to occur right after the sigaltstack() call.
	 */
	catcherState = s;
	sigemptyset (&sa.sa_mask);
	setSigProfHandler (&sa);
	unless (sigaction (SIGPROF, &sa, NULL) == 0)
		diee ("sigaction() failed");
	/* Start the SIGPROF timer. */
	setProfTimer (10000);
}

#endif

/* profileEnd is for writing out an mlmon.out file even if the C code terminates
 * abnormally, e.g. due to running out of memory.  It will only run if the usual
 * SML profile atExit cleanup code did not manage to run.
 */
static GC_state profileEndState;

static void profileEnd () {
	int fd;
	GC_state s;

	if (DEBUG_PROFILE)
		fprintf (stderr, "profileEnd ()\n");
	s = profileEndState;
	if (s->profilingIsOn) {
		fd = creat ("mlmon.out", 0666);
		if (fd < 0)
			diee ("Cannot create mlmon.out");
		GC_profileWrite (s, s->profile, fd);
	}
}

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

static void initSignalStack (GC_state s) {
#if HAS_SIGALTSTACK
        static stack_t altstack;
	size_t ss_size = align (SIGSTKSZ, s->pageSize);
	size_t psize = s->pageSize;
	void *ss_sp = ssmmap (2 * ss_size, psize, psize);
	altstack.ss_sp = ss_sp + ss_size;
	altstack.ss_size = ss_size;
	altstack.ss_flags = 0;
	sigaltstack (&altstack, NULL);
#endif
}

#if FALSE
static bool stringToBool (string s) {
	if (0 == strcmp (s, "false"))
		return FALSE;
	if (0 == strcmp (s, "true"))
		return TRUE;
	die ("Invalid @MLton bool: %s.", s);
}
#endif

static float stringToFloat (string s) {
	float f;

	unless (1 == sscanf (s, "%f", &f))
		die ("Invalid @MLton float: %s.", s);
	return f;
}

static uint stringToBytes (string s) {
	double d;
	char *endptr;
	uint factor;
	
	d = strtod (s, &endptr);
	if (0.0 == d and s == endptr)
		goto bad;
	switch (*endptr++) {
	case 'g':
	case 'G':
		factor = 1024 * 1024 * 1024;
	break;
	case 'k':
	case 'K':
		factor = 1024;
	break;
	case 'm':
	case 'M':
		factor = 1024 * 1024;
	break;
	default:
		goto bad;
	}
	d *= factor;
	unless (strlen (s) == endptr - s
			and (double)INT_MIN <= d 
			and d <= (double)INT_MAX)
		goto bad;
	return (uint)d;
bad:
	die ("Invalid @MLton memory amount: %s.", s);
}

static void setInitialBytesLive (GC_state s) {
	int i;
	int numBytes;
	int numElements;

	s->bytesLive = 0;
	for (i = 0; i < s->intInfInitsSize; ++i) {
		numElements = strlen (s->intInfInits[i].mlstr);
		s->bytesLive +=
			align (GC_ARRAY_HEADER_SIZE 
				+ WORD_SIZE // for the sign
				+ numElements,
				s->alignment);
	}
	for (i = 0; i < s->vectorInitsSize; ++i) {
		numBytes = s->vectorInits[i].bytesPerElement
			* s->vectorInits[i].numElements;
		s->bytesLive +=
			align (GC_ARRAY_HEADER_SIZE
				+ ((0 == numBytes) 
					? POINTER_SIZE
					: numBytes),
				s->alignment);
	}
}

/*
 * For each entry { globalIndex, mlstr } in the inits array (which is terminated
 * by one with an mlstr of NULL), set
 *	state->globals[globalIndex]
 * to the corresponding IntInf.int value.
 * On exit, the GC_state pointed to by s is adjusted to account for any
 * space used.
 */
static void initIntInfs (GC_state s) {
	struct GC_intInfInit *inits;
	pointer frontier;
	char	*str;
	uint	slen,
		llen,
		alen,
		i,
		index;
	bool	neg,
		hex;
	bignum	*bp;
	uchar	*cp;

	assert (isAlignedFrontier (s, s->frontier));
	frontier = s->frontier;
	for (index = 0; index < s->intInfInitsSize; ++index) {
		inits = &s->intInfInits[index];
		str = inits->mlstr;
		assert (inits->globalIndex < s->globalsSize);
		neg = *str == '~';
		if (neg)
			++str;
		slen = strlen (str);
		hex = str[0] == '0' && str[1] == 'x';
		if (hex) {
			str += 2;
			slen -= 2;
			llen = (slen + 7) / 8;
		} else
			llen = (slen + 8) / 9;
		assert (slen > 0);
		bp = (bignum *)frontier;
		cp = (uchar *)&bp->limbs[llen];
		for (i = 0; i != slen; ++i)
			if ('0' <= str[i] && str[i] <= '9')
				cp[i] = str[i] - '0' + 0;
			else if ('a' <= str[i] && str[i] <= 'f')
				cp[i] = str[i] - 'a' + 0xa;
			else {
				assert('A' <= str[i] && str[i] <= 'F');
				cp[i] = str[i] - 'A' + 0xA;
			}
		alen = mpn_set_str (bp->limbs, cp, slen, hex ? 0x10 : 10);
		assert (alen <= llen);
		if (alen <= 1) {
			uint	val,
				ans;

			if (alen == 0)
				val = 0;
			else
				val = bp->limbs[0];
			if (neg) {
				/*
				 * We only fit if val in [1, 2^30].
				 */
				ans = - val;
				val = val - 1;
			} else
				/*
				 * We only fit if val in [0, 2^30 - 1].
				 */
				ans = val;
			if (val < (uint)1<<30) {
				s->globals[inits->globalIndex] = 
					(pointer)(ans<<1 | 1);
				continue;
			}
		}
		s->globals[inits->globalIndex] = (pointer)&bp->isneg;
		bp->counter = 0;
		bp->card = alen + 1;
		bp->magic = BIGMAGIC;
		bp->isneg = neg;
		frontier = alignFrontier (s, (pointer)&bp->limbs[alen]);
	}
	assert (isAlignedFrontier (s, frontier));
	s->frontier = frontier;
	GC_profileAllocInc (s, frontier - s->frontier);
	s->bytesAllocated += frontier - s->frontier;
}

static void initStrings (GC_state s) {
	struct GC_vectorInit *inits;
	pointer frontier;
	int i;

	assert (isAlignedFrontier (s, s->frontier));
	inits = s->vectorInits;
	frontier = s->frontier;
	for (i = 0; i < s->vectorInitsSize; ++i) {
		uint bytesPerElement;
		uint numBytes;
		uint objectSize;
		uint typeIndex;

		bytesPerElement = inits[i].bytesPerElement;
		numBytes = bytesPerElement * inits[i].numElements;
		objectSize = align (GC_ARRAY_HEADER_SIZE
					+ ((0 == numBytes) 
						? POINTER_SIZE
						: numBytes),
					s->alignment);
		assert (objectSize <= s->heap.start + s->heap.size - frontier);
		*(word*)frontier = 0; /* counter word */
		*(word*)(frontier + WORD_SIZE) = inits[i].numElements;
		switch (bytesPerElement) {
		case 1:
			typeIndex = WORD8_VECTOR_TYPE_INDEX;
		break;
		case 2:
			typeIndex = WORD16_VECTOR_TYPE_INDEX;
		break;
		case 4:
			typeIndex = WORD32_VECTOR_TYPE_INDEX;
		break;
		default:
			die ("unknown bytes per element in vectorInit: %d",
				bytesPerElement);
		}
		*(word*)(frontier + 2 * WORD_SIZE) = GC_objectHeader (typeIndex);
		s->globals[inits[i].globalIndex] = 
			frontier + GC_ARRAY_HEADER_SIZE;
		if (DEBUG_DETAILED)
			fprintf (stderr, "allocated string at 0x%x\n",
					(uint)s->globals[inits[i].globalIndex]);
		memcpy (frontier + GC_ARRAY_HEADER_SIZE, inits[i].bytes, 
				numBytes);
		frontier += objectSize;
	}
	if (DEBUG_DETAILED)
		fprintf (stderr, "frontier after string allocation is 0x%08x\n",
				(uint)frontier);
	GC_profileAllocInc (s, frontier - s->frontier);
	s->bytesAllocated += frontier - s->frontier;
	assert (isAlignedFrontier (s, frontier));
	s->frontier = frontier;
}

static void newWorld (GC_state s) {
	int i;
	pointer start;

	for (i = 0; i < s->globalsSize; ++i)
		s->globals[i] = (pointer)BOGUS_POINTER;
	setInitialBytesLive (s);
	heapCreate (s, &s->heap, heapDesiredSize (s, s->bytesLive, 0),
			s->bytesLive);
	createCardMapAndCrossMap (s);
	start = alignFrontier (s, s->heap.start);
	s->frontier = start;
	initIntInfs (s);
	initStrings (s);
	assert (s->frontier - start <= s->bytesLive);
	s->oldGenSize = s->frontier - s->heap.start;
	setNursery (s, 0, 0);
	switchToThread (s, newThreadOfSize (s, initialStackSize (s)));
}

/* worldTerminator is used to separate the human readable messages at the 
 * beginning of the world file from the machine readable data.
 */
static const char worldTerminator = '\000';

static void loadWorld (GC_state s, char *fileName) {
	FILE *file;
	uint magic;
	pointer oldGen;
	int c;
	
	if (DEBUG_WORLD)
		fprintf (stderr, "loadWorld (%s)\n", fileName);
	file = sfopen (fileName, "rb");
	until ((c = fgetc (file)) == worldTerminator or EOF == c);
	if (EOF == c) die ("Invalid world.");
	magic = sfreadUint (file);
	unless (s->magic == magic)
		die ("Invalid world: wrong magic number.");
	oldGen = (pointer) sfreadUint (file);
	s->oldGenSize = sfreadUint (file);
	s->callFromCHandler = (GC_thread) sfreadUint (file);
	s->canHandle = sfreadUint (file);
	s->currentThread = (GC_thread) sfreadUint (file);
	s->signalHandler = (GC_thread) sfreadUint (file);
       	heapCreate (s, &s->heap, heapDesiredSize (s, s->oldGenSize, 0),
			s->oldGenSize);
	createCardMapAndCrossMap (s);
	sfread (s->heap.start, 1, s->oldGenSize, file);
	(*s->loadGlobals) (file);
	unless (EOF == fgetc (file))
		die ("Invalid world: junk at end of file.");
	fclose (file);
	/* translateHeap must occur after loading the heap and globals, since it
	 * changes pointers in all of them.
	 */
	translateHeap (s, oldGen, s->heap.start, s->oldGenSize);
	setNursery (s, 0, 0);
	setStack (s);
}

/* ---------------------------------------------------------------- */
/*                             GC_init                              */
/* ---------------------------------------------------------------- */

Bool MLton_Platform_CygwinUseMmap;

static int processAtMLton (GC_state s, int argc, char **argv, 
				string *worldFile) {
	int i;

	i = 1;
	while (s->mayProcessAtMLton 
		and i < argc 
		and (0 == strcmp (argv [i], "@MLton"))) {
		bool done;

		i++;
		done = FALSE;
		while (!done) {
			if (i == argc)
				die ("Missing -- at end of @MLton args.");
			else {
				string arg;

				arg = argv[i];
				if (0 == strcmp (arg, "copy-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton copy-ratio missing argument.");
					s->copyRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp(arg, "fixed-heap")) {
					++i;
					if (i == argc)
						die ("@MLton fixed-heap missing argument.");
					s->fixedHeap = 
						align (stringToBytes (argv[i++]),
							2 * s->pageSize);
				} else if (0 == strcmp (arg, "gc-messages")) {
					++i;
					s->messages = TRUE;
				} else if (0 == strcmp (arg, "gc-summary")) {
					++i;
#if (defined (__MINGW32__))
					fprintf (stderr, "Warning: MinGW doesn't yet support gc-summary\n");
#else
					s->summary = TRUE;
#endif
				} else if (0 == strcmp (arg, "copy-generational-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton copy-generational-ratio missing argument.");
					s->copyGenerationalRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "grow-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton grow-ratio missing argument.");
					s->growRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "hash-cons")) {
					++i;
					if (i == argc)
						die ("@MLton hash-cons missing argument.");
					s->hashConsFrequency =
						stringToFloat (argv[i++]);
					unless (0.0 <= s->hashConsFrequency
						and s->hashConsFrequency <= 1.0)
						die ("@MLton hash-cons argument must be between 0.0 and 1.0");
				} else if (0 == strcmp (arg, "live-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton live-ratio missing argument.");
					s->liveRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "load-world")) {
					unless (s->mayLoadWorld)
						die ("May not load world.");
					++i;
					s->isOriginal = FALSE;
					if (i == argc) 
						die ("@MLton load-world missing argument.");
					*worldFile = argv[i++];
				} else if (0 == strcmp (arg, "max-heap")) {
					++i;
					if (i == argc) 
						die ("@MLton max-heap missing argument.");
					s->maxHeap = align (stringToBytes (argv[i++]),
								2 * s->pageSize);
				} else if (0 == strcmp (arg, "mark-compact-generational-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton mark-compact-generational-ratio missing argument.");
					s->markCompactGenerationalRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "mark-compact-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton mark-compact-ratio missing argument.");
					s->markCompactRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "no-load-world")) {
					++i;
					s->mayLoadWorld = FALSE;
				} else if (0 == strcmp (arg, "nursery-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton nursery-ratio missing argument.");
					s->nurseryRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "ram-slop")) {
					++i;
					if (i == argc)
						die ("@MLton ram-slop missing argument.");
					s->ramSlop =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "show-prof")) {
					showProf (s);
					exit (0);
				} else if (0 == strcmp (arg, "stop")) {
					++i;
					s->mayProcessAtMLton = FALSE;
				} else if (0 == strcmp (arg, "thread-shrink-ratio")) {
					++i;
					if (i == argc)
						die ("@MLton thread-shrink-ratio missing argument.");
					s->threadShrinkRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "use-mmap")) {
					++i;
					MLton_Platform_CygwinUseMmap = TRUE;
				} else if (0 == strcmp (arg, "--")) {
					++i;
					done = TRUE;
				} else if (i > 1)
					die ("Strange @MLton arg: %s", argv[i]);
			        else done = TRUE;
			}
		}
	}
	return i;
}

int GC_init (GC_state s, int argc, char **argv) {
	char *worldFile;
	int i;

	assert (isAligned (sizeof (struct GC_stack), s->alignment));
	assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread),
				s->alignment));
	assert (isAligned (GC_NORMAL_HEADER_SIZE + sizeof (struct GC_weak),
				s->alignment));
 	MLton_Platform_CygwinUseMmap = FALSE;
	s->amInGC = TRUE;
	s->amInMinorGC = FALSE;
	s->bytesAllocated = 0;
	s->bytesCopied = 0;
	s->bytesCopiedMinor = 0;
	s->bytesMarkCompacted = 0;
	s->callFromCHandler = BOGUS_THREAD;
	s->canHandle = 0;
	s->cardSize = 0x1 << CARD_SIZE_LOG2;
	s->copyRatio = 4.0;
	s->copyGenerationalRatio = 4.0;
	s->currentThread = BOGUS_THREAD;
	s->fixedHeap = 0.0;
	s->gcSignalIsPending = FALSE;
	s->growRatio = 8.0;
	s->handleGCSignal = FALSE;
	s->hashConsDuringGC = FALSE;
	s->hashConsFrequency = 0.0;
	s->inSignalHandler = FALSE;
	s->isOriginal = TRUE;
	s->lastMajor = GC_COPYING;
	s->liveRatio = 8.0;
	s->markCompactRatio = 1.04;
	s->markCompactGenerationalRatio = 8.0;
	s->markedCards = 0;
	s->maxBytesLive = 0;
	s->maxHeap = 0;
	s->maxHeapSizeSeen = 0;
	s->maxPause = 0;
	s->maxStackSizeSeen = 0;
	s->mayLoadWorld = TRUE;
	s->mayProcessAtMLton = TRUE;
	s->messages = FALSE;
	s->minorBytesScanned = 0;
	s->minorBytesSkipped = 0;
	s->numCopyingGCs = 0;
	s->numLCs = 0;
	s->numHashConsGCs = 0;
	s->numMarkCompactGCs = 0;
	s->numMinorGCs = 0;
	s->numMinorsSinceLastMajor = 0;
	s->nurseryRatio = 10.0;
	s->oldGenArraySize = 0x100000;
	s->pageSize = getpagesize ();
	s->ramSlop = 0.5;
	s->rusageIsEnabled = FALSE;
	s->savedThread = BOGUS_THREAD;
	s->signalHandler = BOGUS_THREAD;
	s->signalIsPending = FALSE;
	s->startTime = currentTime ();
	s->summary = FALSE;
	s->threadShrinkRatio = 0.5;
	s->weaks = NULL;
	heapInit (&s->heap);
	heapInit (&s->heap2);
	sigemptyset (&s->signalsHandled);
	initSignalStack (s);
	sigemptyset (&s->signalsPending);
	rusageZero (&s->ru_gc);
	rusageZero (&s->ru_gcCopy);
	rusageZero (&s->ru_gcMarkCompact);
	rusageZero (&s->ru_gcMinor);
	worldFile = NULL;
	unless (isAligned (s->pageSize, s->cardSize))
		die ("Page size must be a multiple of card size.");
	processAtMLton (s, s->atMLtonsSize, s->atMLtons, &worldFile);
	i = processAtMLton (s, argc, argv, &worldFile);
	if (s->fixedHeap > 0 and s->maxHeap > 0)
		die ("Cannot use both fixed-heap and max-heap.\n");
	unless (ratiosOk (s))
		die ("invalid ratios");
	s->totalRam = totalRam (s);
	/* We align s->ram by pageSize so that we can test whether or not we
	 * we are using mark-compact by comparing heap size to ram size.  If 
	 * we didn't round, the size might be slightly off.
         */
	s->ram = align (s->ramSlop * s->totalRam, s->pageSize);
	if (DEBUG or DEBUG_RESIZING or s->messages)
		fprintf (stderr, "total RAM = %s  RAM = %s\n",
				uintToCommaString (s->totalRam), 
				uintToCommaString (s->ram));
	if (DEBUG_PROFILE) {
		int i;
			for (i = 0; i < s->frameSourcesSize; ++i) {
			int j;
			uint *sourceSeq;
				fprintf (stderr, "%d\n", i);
			sourceSeq = s->sourceSeqs[s->frameSources[i]];
			for (j = 1; j <= sourceSeq[0]; ++j)
				fprintf (stderr, "\t%s\n",
						s->sourceNames[s->sources[sourceSeq[j]].nameIndex]);
		}
	}
	/* Initialize profiling.  This must occur after processing command-line 
         * arguments, because those may just be doing a show prof, in which 
         * case we don't want to initialize the atExit.
         */
	if (PROFILE_NONE == s->profileKind)
		s->profilingIsOn = FALSE;
	else {
		s->profilingIsOn = TRUE;
		assert (s->frameSourcesSize == s->frameLayoutsSize);
		switch (s->profileKind) {
		case PROFILE_ALLOC:
		case PROFILE_COUNT:
			s->profile = GC_profileNew (s);
		break;
		case PROFILE_NONE:
			die ("impossible PROFILE_NONE");
		case PROFILE_TIME:
			profileTimeInit (s);
		break;
		}
		profileEndState = s;
		atexit (profileEnd);
	}
	if (s->isOriginal) {
		newWorld (s);
		/* The mutator stack invariant doesn't hold,
		 * because the mutator has yet to run.
		 */
		assert (mutatorInvariant (s, TRUE, FALSE));
	} else {
		loadWorld (s, worldFile);
		if (s->profilingIsOn and s->profileStack)
			GC_foreachStackFrame (s, enterFrame);
		assert (mutatorInvariant (s, TRUE, TRUE));
	}
	s->amInGC = FALSE;
	return i;
}

extern char **environ; /* for Posix_ProcEnv_environ */

void MLton_init (int argc, char **argv, GC_state s) {
	int start;

	Posix_ProcEnv_environ = (CstringArray)environ;
	start = GC_init (s, argc, argv);
	/* Setup argv and argc that SML sees. */
	/* start is now the index of the first real arg. */
	CommandLine_commandName = (uint)(argv[0]);
	CommandLine_argc = argc - start;
	CommandLine_argv = (uint)(argv + start);
}

static void displayCol (FILE *out, int width, string s) {
	int extra;
	int i;
	int len;

	len = strlen (s);
	if (len < width) {
	        extra = width - len;
		for (i = 0; i < extra; ++i)
			fprintf (out, " ");
	}
	fprintf (out, "%s\t", s);
}

static void displayCollectionStats (FILE *out, string name, struct rusage *ru, 
					uint num, ullong bytes) {
	uint ms;

	ms = rusageTime (ru);
	fprintf (out, "%s", name);
	displayCol (out, 7, uintToCommaString (ms));
	displayCol (out, 7, uintToCommaString (num));
	displayCol (out, 15, ullongToCommaString (bytes));
	displayCol (out, 15, 
			(ms > 0)
			? uintToCommaString (1000.0 * (float)bytes/(float)ms)
			: "-");
	fprintf (out, "\n");
}

void GC_done (GC_state s) {
	FILE *out;

	enter (s);
	minorGC (s);
	out = stderr;
	if (s->summary) {
		double time;
		uint gcTime;

		gcTime = rusageTime (&s->ru_gc);
		fprintf (out, "GC type\t\ttime ms\t number\t\t  bytes\t      bytes/sec\n");
		fprintf (out, "-------------\t-------\t-------\t---------------\t---------------\n");
		displayCollectionStats
			(out, "copying\t\t", &s->ru_gcCopy, s->numCopyingGCs, 
				s->bytesCopied);
		displayCollectionStats
			(out, "mark-compact\t", &s->ru_gcMarkCompact, 
				s->numMarkCompactGCs, s->bytesMarkCompacted);
		displayCollectionStats
			(out, "minor\t\t", &s->ru_gcMinor, s->numMinorGCs, 
				s->bytesCopiedMinor);
		time = (double)(currentTime () - s->startTime);
		fprintf (out, "total GC time: %s ms (%.1f%%)\n",
				intToCommaString (gcTime), 
				(0.0 == time) 
					? 0.0 
					: 100.0 * ((double) gcTime) / time);
		fprintf (out, "max pause: %s ms\n",
				uintToCommaString (s->maxPause));
		fprintf (out, "total allocated: %s bytes\n",
	 			ullongToCommaString (s->bytesAllocated));
		fprintf (out, "max live: %s bytes\n",
				uintToCommaString (s->maxBytesLive));
		fprintf (out, "max semispace: %s bytes\n", 
				uintToCommaString (s->maxHeapSizeSeen));
		fprintf (out, "max stack size: %s bytes\n", 
				uintToCommaString (s->maxStackSizeSeen));
		fprintf (out, "marked cards: %s\n", 
				ullongToCommaString (s->markedCards));
		fprintf (out, "minor scanned: %s bytes\n",
				uintToCommaString (s->minorBytesScanned));
		fprintf (out, "minor skipped: %s bytes\n", 
				uintToCommaString (s->minorBytesSkipped));
	}
	heapRelease (s, &s->heap);
	heapRelease (s, &s->heap2);
}

void GC_finishHandler (GC_state s) {
	if (DEBUG_SIGNALS)
		fprintf (stderr, "GC_finishHandler ()\n");
	assert (s->canHandle == 1);
	s->inSignalHandler = FALSE;	
}

/* GC_handler sets s->limit = 0 so that the next limit check will fail. 
 * Signals need to be blocked during the handler (i.e. it should run atomically)
 * because sigaddset does both a read and a write of s->signalsPending.
 * The signals are blocked by Posix_Signal_handle (see Posix/Signal/Signal.c).
 */
void GC_handler (GC_state s, int signum) {
	if (DEBUG_SIGNALS)
		fprintf (stderr, "GC_handler signum = %d\n", signum);
	assert (sigismember (&s->signalsHandled, signum));
	if (s->canHandle == 0)
		s->limit = 0;
	s->signalIsPending = TRUE;
	sigaddset (&s->signalsPending, signum);
	if (DEBUG_SIGNALS)
		fprintf (stderr, "GC_handler done\n");
}

uint GC_size (GC_state s, pointer root) {
	uint res;

	if (DEBUG_SIZE)
		fprintf (stderr, "GC_size marking\n");
	res = mark (s, root, MARK_MODE, FALSE);
	if (DEBUG_SIZE)
		fprintf (stderr, "GC_size unmarking\n");
	mark (s, root, UNMARK_MODE, FALSE);
	return res;
}

void GC_saveWorld (GC_state s, int fd) {
	char buf[80];

	if (DEBUG_WORLD)
		fprintf (stderr, "GC_saveWorld (%d).\n", fd);
	enter (s);
	/* Compact the heap. */
	doGC (s, 0, 0, TRUE, TRUE);
	sprintf (buf,
		"Heap file created by MLton.\nheap.start = 0x%08x\nbytesLive = %u\n",
		(uint)s->heap.start, (uint)s->bytesLive);
	swrite (fd, buf, 1 + strlen(buf)); /* +1 to get the '\000' */
	swriteUint (fd, s->magic);
	swriteUint (fd, (uint)s->heap.start);
	swriteUint (fd, (uint)s->oldGenSize);
	swriteUint (fd, (uint)s->callFromCHandler);
	/* canHandle must be saved in the heap, because the saveWorld may be
	 * run in the context of a critical section, which will expect to be in	
	 * the same context when it is restored.
	 */
	swriteUint (fd, s->canHandle);
	swriteUint (fd, (uint)s->currentThread);
	swriteUint (fd, (uint)s->signalHandler);
 	swrite (fd, s->heap.start, s->oldGenSize);
	(*s->saveGlobals) (fd);
	leave (s);
}

void GC_pack (GC_state s) {
	uint keep;

	enter (s);
	if (DEBUG or s->messages)
		fprintf (stderr, "Packing heap of size %s.\n",
				uintToCommaString (s->heap.size));
	/* Could put some code here to skip the GC if there hasn't been much
	 * allocated since the last collection.  But you would still need to 
	 * do a minor GC to make all objects contiguous.
 	 */
	doGC (s, 0, 0, TRUE, FALSE);
	keep = s->oldGenSize * 1.1;
	if (keep <= s->heap.size) {
		heapShrink (s, &s->heap, keep);
		setNursery (s, 0, 0);
		setStack (s);
	}
	heapRelease (s, &s->heap2);
	if (DEBUG or s->messages)
		fprintf (stderr, "Packed heap to size %s.\n",
				uintToCommaString (s->heap.size));
	leave (s);
}

void GC_unpack (GC_state s) {
	enter (s);
	if (DEBUG or s->messages)
		fprintf (stderr, "Unpacking heap of size %s.\n",
				uintToCommaString (s->heap.size));
	/* The enterGC is needed here because minorGC and resizeHeap might move
	 * the stack, and the SIGPROF catcher would then see a bogus stack.  The
 	 * leaveGC has to happen after the setStack.
	 */
	enterGC (s);
	minorGC (s);
	resizeHeap (s, s->oldGenSize);
	resizeHeap2 (s);
	setNursery (s, 0, 0);
	setStack (s);
	leaveGC (s);
	if (DEBUG or s->messages)
		fprintf (stderr, "Unpacked heap to size %s.\n",
				uintToCommaString (s->heap.size));
	leave (s);
}

/* ------------------------------------------------- */
/*                     GC_weak*                      */
/* ------------------------------------------------- */

/* A weak object is a header followed by two words.
 *
 * The object type indexed by the header determines whether the weak is valid
 * or not.  If the type has numPointers == 1, then the weak pointer is valid.  
 * Otherwise, the type has numPointers == 0 and the weak pointer is not valid.
 *
 * The first word is used to chain the live weaks together during a copying gc
 * and is otherwise unused.
 *
 * The second word is the weak pointer.
 */ 

bool GC_weakCanGet (pointer p) {
	Bool res;

	res = WEAK_GONE_HEADER != GC_getHeader (p);
	if (DEBUG_WEAK)
		fprintf (stderr, "%s = GC_weakCanGet (0x%08x)\n",
				boolToString (res), (uint)p);
	return res;
}

Pointer GC_weakGet (Pointer p) {
	pointer res;

	res = ((GC_weak)p)->object;
	if (DEBUG_WEAK)
		fprintf (stderr, "0x%08x = GC_weakGet (0x%08x)\n",
				(uint)res, (uint)p);
	return res;
}

Pointer GC_weakNew (GC_state s, Word32 header, Pointer p) {
	pointer res;

	res = object (s, header, GC_NORMAL_HEADER_SIZE + 3 * WORD_SIZE, 
			FALSE, FALSE);
	((GC_weak)res)->object = p;
	if (DEBUG_WEAK)
		fprintf (stderr, "0x%08x = GC_weakNew (0x%08x, 0x%08x)\n",
				(uint)res, (uint)header, (uint)p);
	return res;
}
