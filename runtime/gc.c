/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */

#include "gc.h"
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/sysinfo.h>
#include <sys/times.h>
#include <time.h>
#include <values.h>

#define METER FALSE  /* Displays distribution of object sizes at program exit. */
/* 
 * Object headers.
 *
 * bit 31 == 1
 *   normal object
 * bit 31 == 0
 *   bit 29 == 0
 *     array
 *   bit 29 == 1
 *     bit 28 == 0
 *       stack
 *     bit 28 == 1
 *       cont
 *
 * The only difference between conts and stacks is that the former has 
 *   used == reserved.
 *
 * bit 30 is used as a mark bit.  For now, it is only used in GC_size.
 */

/* The mutator should maintain the invariants
 *
 *  function entry: stackTop + maxFrameSize <= endOfStack
 *  anywhere else: stackTop + 2 * maxFrameSize <= endOfStack
 * 
 * The latter will give it enough space to make a function call and always
 * satisfy the former.  The former will allow it to make a gc call at the
 * function entry limit.
 */

enum {
	BOGUS_EXN_STACK = 0xFFFFFFFF,
	BOGUS_POINTER = 0x1,
	DEBUG = FALSE,
	FORWARDED = 0xFFFFFFFF,
	HEADER_SIZE = WORD_SIZE,
	LIMIT_SLOP = 512,
	STACK_HEADER = 0x30000000,
	CONT_HEADER = 0x20000000,
	STACK_HEADER_SIZE = WORD_SIZE,
};

#define BOGUS_THREAD (GC_thread)BOGUS_POINTER
#define STRING_HEADER GC_arrayHeader(1, 0)
#define WORD8_VECTOR_HEADER GC_arrayHeader(1, 0)
#define THREAD_HEADER GC_objectHeader(2, 1)

static void leave(GC_state s);

static inline void splitHeader(word header, uint *numPointers, 
				uint *numNonPointers) {
	*numPointers = header & ONES(POINTER_BITS);
	*numNonPointers = 
		(header >> NON_POINTERS_SHIFT) & ONES(NON_POINTER_BITS);
}

static inline bool isNormal(word header) {
	return header & HIGH_BIT;
}

static inline bool isStackOrContHeader(word header) {
	assert(not(isNormal(header)));
	return header & 0x20000000;
}

static inline uint toBytes(uint n) {
	return n << 2;
}

static inline bool isWordAligned(uint x) {
	return 0 == (x & 0x3);
}

static inline uint min(uint x, uint y) {
	return ((x < y) ? x : y);
}

static inline uint max(uint x, uint y) {
	return ((x > y) ? x : y);
}

/* A super-safe mmap.
 *  Allocates a region of memory with dead zones at the high and low ends.
 *  Any attempt to touch the dead zone (read or write) will cause a
 *   segmentation fault.
 */
static void *ssmmap(size_t length, size_t dead_low, size_t dead_high) {
  void *base,*low,*result,*high;

  base = smmap(length + dead_low + dead_high);

  low = base;
  if (mprotect(low, dead_low, PROT_NONE))
    diee("mprotect failed");

  result = low + dead_low;
  high = result + length;

  if (mprotect(high, dead_high, PROT_NONE))
    diee("mprotect failed");

  return result;
}

/* ------------------------------------------------- */
/*                     roundPage                     */
/* ------------------------------------------------- */

/*
 * Round size up to a multiple of the size of a page.
 */
static inline size_t
roundPage(size_t size)
{
	static size_t	psize;

	psize = getpagesize();
	size += psize - 1;
	size -= size % psize;
	return (size);
}

/* ------------------------------------------------- */
/*                      display                      */
/* ------------------------------------------------- */

void GC_display(GC_state s, FILE *stream) {
	fprintf(stream, "GC state\n\tbase = %x  frontier - base = %d  limit - frontier = %d\n",
			(uint) s->base, 
			s->frontier - s->base,
			s->limit - s->frontier);
	fprintf(stream, "\tcanHandle = %d\n", s->canHandle);
	fprintf(stream, "\texnStack = %d  bytesNeeded = %d  reserved = %d  used = %d\n",
			s->currentThread->exnStack,
			s->currentThread->bytesNeeded,
			s->currentThread->stack->reserved,
			s->currentThread->stack->used);
	fprintf(stream, "\tstackBottom = %x\nstackTop - stackBottom = %d\nstackLimit - stackTop = %d\n",
			(uint)s->stackBottom,
			s->stackTop - s->stackBottom,
			(s->stackLimit - s->stackTop));
}

/* ------------------------------------------------- */
/*                    ensureFree                     */
/* ------------------------------------------------- */

static inline void
ensureFree(GC_state s, uint bytesRequested)
{
	if (s->frontier + bytesRequested > s->limit) {
		GC_doGC(s, bytesRequested, 0);
	}
}

/* ------------------------------------------------- */
/*                      object                       */
/* ------------------------------------------------- */

static inline pointer
object(GC_state s, uint header, uint bytesRequested)
{
	pointer result;

	assert(s->frontier + bytesRequested <= s->limit);
	assert(isWordAligned(bytesRequested));
	*(uint*)s->frontier = header;
	result = s->frontier + HEADER_SIZE;
	s->frontier += bytesRequested;
	return result;
}

/* ------------------------------------------------- */
/*                  getFrameLayout                   */
/* ------------------------------------------------- */

static inline GC_frameLayout	*
getFrameLayout(GC_state s, word returnAddress)
{
	GC_frameLayout *layout;
	uint index;

	if (s->native)
		index = *((uint*)(returnAddress - 4));
	else
		index = (uint)returnAddress;
	assert(0 <= index && index <= s->maxFrameIndex);
	layout = &(s->frameLayouts[index]);
	assert(layout->numBytes > 0);
	return layout;
}

/* ------------------------------------------------- */
/*                      Stacks                       */
/* ------------------------------------------------- */

/* stackSlop returns the amount of "slop" space needed between the top of 
 * the stack and the end of the stack space.
 * If you change this, make sure and change Thread_switchTo in ccodegen.h
 *   and thread_switchTo in x86-generate-transfers.sml.
 */
static inline uint stackSlop(GC_state s) {
	return 2 * s->maxFrameSize;
}

static inline uint initialStackSize(GC_state s) {
	return stackSlop(s);
}

static inline uint
stackBytes(uint size)
{
	return wordAlign(HEADER_SIZE + sizeof(struct GC_stack) + size);
}

/* If you change this, make sure and change Thread_switchTo in ccodegen.h
 *   and thread_switchTo in x86-generate-transfers.sml.
 */
static inline pointer
stackBottom(GC_stack stack)
{
	return ((pointer)stack) + sizeof(struct GC_stack);
}

/* Pointer to the topmost word in use on the stack. */
/* If you change this, make sure and change Thread_switchTo in ccodegen.h
 *   and thread_switchTo in x86-generate-transfers.sml.
 */
static inline pointer
stackTop(GC_stack stack)
{
	return stackBottom(stack) + stack->used;
}

/* The maximum value stackTop may take on. */
/* If you change this, make sure and change Thread_switchTo in ccodegen.h
 *   and thread_switchTo in x86-generate-transfers.sml.
 */
static inline pointer
stackLimit(GC_state s, GC_stack stack)
{
	return stackBottom(stack) + stack->reserved - stackSlop(s);
}

/* Number of bytes used by the stack. */
/* If you change this, make sure and change Thread_switchTo in ccodegen.h
 *   and thread_switchTo in x86-generate-transfers.sml.
 */
static inline uint
currentStackUsed(GC_state s)
{
	return s->stackTop - s->stackBottom;
}

static inline bool
stackIsEmpty(GC_stack stack)
{
	return 0 == stack->used;
}

static inline uint
topFrameSize(GC_state s, GC_stack stack)
{
	GC_frameLayout *layout;
	
	assert(not(stackIsEmpty(stack)));
	layout = getFrameLayout(s, *(word*)(stackTop(stack) - WORD_SIZE));
	return layout->numBytes;
}

/* stackTopIsOk ensures that when this stack becomes current that 
 * the stackTop is less than the stackLimit.
 */
static inline bool
stackTopIsOk(GC_state s, GC_stack stack)
{
	return stackTop(stack) 
		       	<= stackLimit(s, stack) 
			+ (stackIsEmpty(stack) ? 0 : topFrameSize(s, stack));
}

static inline GC_stack
newStack(GC_state s, word header, uint size)
{
	GC_stack stack;

	stack = (GC_stack)object(s, header, stackBytes(size));
	stack->reserved = size;
	stack->used = 0;
	return stack;
}

inline void
GC_setStack(GC_state s)
{
	GC_stack stack;

	stack = s->currentThread->stack;
	s->stackBottom = stackBottom(stack);
	s->stackTop = stackTop(stack);
	s->stackLimit = stackLimit(s, stack);
}

static inline void
stackCopy(GC_stack from, GC_stack to)
{
	assert(from->used <= to->reserved);
	to->used = from->used;
	memcpy(stackBottom(to), stackBottom(from), from->used);
}

/* ------------------------------------------------- */
/*                GC_computeHeapSize                 */
/* ------------------------------------------------- */

/* GC_computeHeapSize returns min(s->maxHeapSize, live * ratio).
 * It is careful not to overflow when doing the multiply.
 * It should only be called when not s->useFixedHeap, since s->maxHeapSize is
 * only set in that case.
 */
static uint
computeHeapSize(GC_state s, uint live, uint ratio)
{
	ullong needed;

	assert(not s->useFixedHeap);
	needed = ((ullong)live) * ratio;
	return ((needed > (ullong)s->maxHeapSize)
		? s->maxHeapSize
		: roundPage(needed));
}

/* ------------------------------------------------- */
/*               extractArrayNumBytes                */
/* ------------------------------------------------- */

/* The number of bytes in an array, not including the header. */
static inline uint
extractArrayNumBytes(pointer p, 
		     uint numPointers,
		     uint numNonPointers)
{
	uint numElements, bytesPerElement, result;
	
	numElements = GC_arrayNumElements(p);
	bytesPerElement = numNonPointers + toBytes(numPointers);
	result = wordAlign(numElements * bytesPerElement);
	
	return result;
}

static inline void
maybeCall(GC_pointerFun f, GC_state s, pointer *pp)
{
	if (GC_isPointer(*pp))
		f(s, pp);
}

/* ------------------------------------------------- */
/*                 GC_foreachGlobal                  */
/* ------------------------------------------------- */

/* Apply f to each global pointer into the heap. */
inline void
GC_foreachGlobal(GC_state s, GC_pointerFun f)
{
	int i;

 	for (i = 0; i < s->numGlobals; ++i)
		maybeCall(f, s, &s->globals[i]);
	maybeCall(f, s, (pointer*)&s->currentThread);
	maybeCall(f, s, (pointer*)&s->savedThread);
	maybeCall(f, s, (pointer*)&s->signalHandler);
}

/* ------------------------------------------------- */
/*             GC_foreachPointerInObject             */
/* ------------------------------------------------- */
/*
 * Apply f to each pointer in the object p, where p points at the first
 * data word in the object.
 * Returns pointer to the end of object, i.e. just past object.
 */
inline pointer
GC_foreachPointerInObject(GC_state s, GC_pointerFun f, pointer p)
{
	word header;
	uint numPointers;
	uint numNonPointers;

	header = GC_getHeader(p);
	if (isNormal(header)) { /* It's a normal object. */
		pointer max;

		splitHeader(header, &numPointers, &numNonPointers);
		p += toBytes(numNonPointers);
		max = p + toBytes(numPointers);
		/* Apply f to all internal pointers. */
		for ( ; p < max; p += POINTER_SIZE) 
			maybeCall(f, s, (pointer*)p);
	} else if (isStackOrContHeader(header)) {
		GC_stack stack;
		pointer top, bottom;
		int i;
		word returnAddress;
		GC_frameLayout *layout;
		GC_offsets frameOffsets;

		stack = (GC_stack)p;
		bottom = stackBottom(stack);
		top = stackTop(stack);
		assert(stack->used <= stack->reserved);
		while (top > bottom) {
			/* Invariant: top points just past a "return address". */
			returnAddress = *(word*) (top - WORD_SIZE);
			if (DEBUG)
				fprintf(stderr, 
					"  top = %d  return address = %d.\n", 
					top - bottom, 
					returnAddress);
			layout = getFrameLayout(s, returnAddress); 
			frameOffsets = layout->offsets;
			top -= layout->numBytes;
			for (i = 0 ; i < frameOffsets[0] ; ++i) {
				if (DEBUG)
					fprintf(stderr, 
						"    offset %d  address %x\n", 
						frameOffsets[i + 1],
						(uint)(*(pointer*)(top + frameOffsets[i + 1])));
				maybeCall(f, s, 
					  (pointer*)
					  (top + frameOffsets[i + 1]));
			}
		}
		assert(top == bottom);
		p += sizeof(struct GC_stack) + stack->reserved;
	} else { /* It's an array. */
		uint numBytes;

		splitHeader(header, &numPointers, &numNonPointers);
		numBytes = extractArrayNumBytes(p, numPointers, numNonPointers);
		if (numBytes == 0)
			/* An empty array -- skip the POINTER_SIZE bytes
			 * for the forwarding pointer.
			 */
			p += POINTER_SIZE;
		else {
			pointer max;

			max = p + numBytes;
			if (numPointers == 0) {
				/* There are no pointers, just update p. */
				p = max;
			} else if (numNonPointers == 0) {
			  	/* It's an array with only pointers. */
				for (; p < max; p += POINTER_SIZE)
					maybeCall(f, s, (pointer*)p);
			} else {
				uint numBytesPointers;
				
				numBytesPointers = toBytes(numPointers);
				/* For each array element. */
				while (p < max) {
					pointer max2;

					p += numNonPointers;
					max2 = p + numBytesPointers;
					/* For each internal pointer. */
					for ( ; p < max2; p += POINTER_SIZE) 
						maybeCall(f, s, (pointer*)p);
				}
			}
			assert(p == max);
		}
	}
	return p;
}

/* ------------------------------------------------- */
/*                      toData                       */
/* ------------------------------------------------- */

/* p should point at the beginning of an object (i.e. the header).
 * Returns a pointer to the start of the object data.
 */
static inline pointer
toData(pointer p)
{
	word header;	

	header = *(word*)p;

	return ((isNormal(header) or isStackOrContHeader(header))
		? p + WORD_SIZE
		: p + 2 * WORD_SIZE);
}

/* ------------------------------------------------- */
/*             GC_foreachPointerInRange              */
/* ------------------------------------------------- */

/* Apply f to each pointer between front and *back, which should be a 
 * contiguous sequence of objects, where front points at the beginning of
 * the first object and *back points just past the end of the last object.
 * f may increase *back (for example, this is done by forward).
 */

inline void
GC_foreachPointerInRange(GC_state s, pointer front, pointer *back,
			 GC_pointerFun f)
{
	pointer b;

	b = *back;
	assert(front <= b);
 	while (front < b) {
		while (front < b) {
			assert(isWordAligned((uint)front));
			front = GC_foreachPointerInObject(s, f, toData(front));
		}
		b = *back;
	}
	assert(front == *back);
}

/* ------------------------------------------------- */
/*                     invariant                     */
/* ------------------------------------------------- */

#ifndef NODEBUG

static inline bool
isInFromSpace(GC_state s, pointer p)
{
 	return (s->base <= p and p < s->frontier);
}

static inline void
assertIsInFromSpace(GC_state s, pointer *p)
{
	assert(isInFromSpace(s, *p));
}

static inline bool
isInToSpace(GC_state s, pointer p)
{
	return (not(GC_isPointer(p))
		or (s->toBase <= p and p < s->toBase + s->toSize));
}

static bool
invariant(GC_state s)
{
	/* would be nice to add divisiblity by pagesize of various things */

	/* Frame layouts */
	{	
		int i;

		for (i = 0; i < s->maxFrameIndex; ++i) {
			GC_frameLayout *layout;

 			layout = &(s->frameLayouts[i]);
			if (layout->numBytes > 0) {
				GC_offsets offsets;
				int j;

				assert(layout->numBytes <= s->maxFrameSize);
				offsets = layout->offsets;
				for (j = 0; j < offsets[0]; ++j)
					assert(offsets[j + 1] < layout->numBytes);
			}
		}
	}
	/* Heap */
	assert(isWordAligned((uint)s->frontier));
	assert(s->base <= s->frontier);
	assert(0 == s->fromSize 
		or (s->frontier <= s->limit + LIMIT_SLOP
			and s->limit == s->base + s->fromSize - LIMIT_SLOP));
	assert(s->useFixedHeap or (s->fromSize <= s->maxHeapSize
	                           and s->toSize <= s->maxHeapSize));
	assert(s->toBase == NULL or s->toSize == s->fromSize);
	/* Check that all pointers are into from space. */
	GC_foreachGlobal(s, assertIsInFromSpace);
	GC_foreachPointerInRange(s, s->base, &s->frontier, assertIsInFromSpace);
	/* Current thread. */
	{
/*		uint offset; */
		GC_stack stack;

		stack = s->currentThread->stack;
		assert(isWordAligned(stack->reserved));
		assert(s->stackBottom == stackBottom(stack));
		assert(s->stackTop == stackTop(stack));
	 	assert(s->stackLimit == stackLimit(s, stack));
		assert(stack->used == currentStackUsed(s));
		assert(stack->used < stack->reserved);
	 	assert(s->stackBottom <= s->stackTop);
/* Can't walk down the exception stack these days, because there is no 
 * guarantee that the handler link and slot are next to each other.
 */
/* 		for (offset = s->currentThread->exnStack;  */
/* 			offset != BOGUS_EXN_STACK; ) { */
/* 			unless (s->stackBottom + offset <= s->stackTop) */
/* 				fprintf(stderr, "s->stackBottom = %d  offset = %d s->stackTop = %d\n", (uint)(s->stackBottom), offset, (uint)(s->stackTop)); */
/* 			assert(s->stackBottom + offset <= s->stackTop); */
/* 			offset = *(uint*)(s->stackBottom + offset + WORD_SIZE); */
/* 		} */
	}

	return TRUE;
}

bool
GC_mutatorInvariant(GC_state s)
{
	if (DEBUG)
		GC_display(s, stderr);
	assert(stackTopIsOk(s, s->currentThread->stack));
	assert(invariant(s));
	return TRUE;
}
#endif /* #ifndef NODEBUG */

/* ------------------------------------------------- */
/*                      Threads                      */
/* ------------------------------------------------- */

static inline uint
threadBytes()
{
	return wordAlign(HEADER_SIZE + sizeof(struct GC_thread));
}

static inline uint
initialThreadBytes(GC_state s)
{
	return threadBytes() + stackBytes(initialStackSize(s));
}

static inline GC_thread
newThreadOfSize(GC_state s, word header, uint stackSize)
{
	GC_stack stack;
	GC_thread t;

	ensureFree(s, stackBytes(stackSize) + threadBytes());
	stack = newStack(s, header, stackSize);
	t = (GC_thread)object(s, THREAD_HEADER, threadBytes());
	t->exnStack = BOGUS_EXN_STACK;
	t->stack = stack;
	return t;
}

static inline void
switchToThread(GC_state s, GC_thread t)
{
	s->currentThread = t;
	GC_setStack(s);
}

static inline void
copyThread(GC_state s, GC_thread from, word header, uint size)
{
	GC_thread to;

	/* newThreadOfSize may do a GC, which invalidates from.  
	 * Hence we need to stash from where the GC can find it.
	 */
	s->savedThread = from;
	to = newThreadOfSize(s, header, size);
	from = s->savedThread;
	stackCopy(from->stack, to->stack);
	to->exnStack = from->exnStack;
	s->savedThread = to;
}

/* ------------------------------------------------- */
/*                fromSpace, toSpace                 */
/* ------------------------------------------------- */

static inline void setLimit(GC_state s) {
	s->limit = s->base + s->fromSize - LIMIT_SLOP;
}

/* ------------------------------------------------- */
/*                      Signals                      */
/* ------------------------------------------------- */

static inline void
blockSignals(GC_state s)
{
	sigprocmask(SIG_BLOCK, &s->signalsHandled, NULL);
}

static inline void
unblockSignals(GC_state s)
{
	sigprocmask(SIG_UNBLOCK, &s->signalsHandled, NULL);
}

/* enter and leave should be called at the start and end of every GC function
 * that is exported to the outside world.  They make sure that signals are
 * blocked for the duration of the function and check the GC invariant
 * They are a bit tricky because of the case when the runtime system is invoked
 * from within an ML signal handler.
 */
inline void
GC_enter(GC_state s)
{
	/* used needs to be set because the mutator has changed s->stackTop. */
	s->currentThread->stack->used = currentStackUsed(s);
	if (DEBUG) 
		GC_display(s, stderr);
	unless (s->inSignalHandler) {
		blockSignals(s);
		if (s->limit == 0)
			setLimit(s);
	}
	assert(invariant(s));
}

static inline void
leave(GC_state s)
{
	assert(GC_mutatorInvariant(s));
	if (s->signalIsPending and 0 == s->canHandle)
		s->limit = 0;
	unless (s->inSignalHandler)
		unblockSignals(s);
}

inline void
GC_copyCurrentThread(GC_state s)
{
	GC_thread t;

	GC_enter(s);
	t = s->currentThread;
	copyThread(s, t, CONT_HEADER, t->stack->used);
	assert(s->frontier <= s->limit);
	leave(s);
}

static inline uint
stackNeedsReserved(GC_state s, GC_stack stack)
{
	return stack->used + stackSlop(s) - topFrameSize(s, stack);
}

inline void
GC_copyThread(GC_state s, GC_thread t)
{
	GC_enter (s);
	assert (t->stack->reserved == t->stack->used);
	copyThread (s, t, STACK_HEADER, stackNeedsReserved(s, t->stack));
	assert(s->frontier <= s->limit);
	leave(s);
}

extern struct GC_state gcState;

inline void
Thread_atomicBegin()
{
	if (DEBUG)
		fprintf(stderr, "atomicBegin %d -> %d\n", 
				gcState.canHandle, gcState.canHandle + 1);
	assert(gcState.canHandle >= 0);
 	gcState.canHandle++;
	if (gcState.signalIsPending)
		setLimit(&gcState);
}

/* Warning: If you change this, you must also change Thread_switchTo in
 * ccodegen.h and the implementation of Thread_switchTo in 
 * x86-generate-transfers.fun.
 */
inline void
Thread_atomicEnd()
{
	if (DEBUG)
		fprintf(stderr, "atomicEnd %d -> %d\n", 
				gcState.canHandle, gcState.canHandle - 1);
	gcState.canHandle--;
	assert(gcState.canHandle >= 0);
	if (gcState.signalIsPending && 0 == gcState.canHandle)
		gcState.limit = 0;
}

inline void
GC_fromSpace(GC_state s)
{
	s->base = smmap(s->fromSize);
	if (s->fromSize > s->maxHeapSizeSeen)
		s->maxHeapSizeSeen = s->fromSize;
	setLimit(s);
}

static void *smmap_dienot(size_t length, int direction) {
	int i;
	void *result = (void*)-1;

	for (i = 0; i < 32; i++) {
		unsigned long address;

		address = i*0x08000000ul;
		if (direction)
			address = 0xf8000000ul - address;
		result = mmap(address+(void*)0, length, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANON, -1, 0);
	  if ((void*)-1 != result)
		break;
	}
	return result;
}

static void 
toSpace (GC_state s) {
	static int direction = 1;
	s->toBase = smmap_dienot(s->toSize, direction);
	if (s->toBase != (void*)-1) {
		direction = (direction==0);
		if (s->toSize > s->maxHeapSizeSeen)
			s->maxHeapSizeSeen = s->toSize;
	}
}

void 
GC_toSpace(GC_state s)
{
	toSpace(s);
 	if (s->toBase == (void*)-1) 
		diee("Out of swap space");
}

/* ------------------------------------------------- */
/*                    getrusage                      */
/* ------------------------------------------------- */

int
fixedGetrusage(int who, struct rusage *rup)
{
	struct tms	tbuff;
	int		res;
	clock_t		user,
			sys;
	static bool	first = TRUE;
	static long	hz;

	if (first) {
		first = FALSE;
		hz = sysconf(_SC_CLK_TCK);
	}
	res = getrusage(who, rup);
	unless (res == 0)
		return (res);
	if (times(&tbuff) == -1)
		diee("Impossible: times() failed");
	switch (who) {
	case RUSAGE_SELF:
		user = tbuff.tms_utime;
		sys = tbuff.tms_stime;
		break;
	case RUSAGE_CHILDREN:
		user = tbuff.tms_cutime;
		sys = tbuff.tms_cstime;
		break;
	default:
		die("getrusage() accepted unknown who: %d", who);
		exit(1);  /* needed to keep gcc from whining. */
	}
	rup->ru_utime.tv_sec = user / hz;
	rup->ru_utime.tv_usec = (user % hz) * (1000000 / hz);
	rup->ru_stime.tv_sec = sys / hz;
	rup->ru_stime.tv_usec = (sys % hz) * (1000000 / hz);
	return (0);
}

static inline void
rusageZero(struct rusage *ru)
{
	memset(ru, 0, sizeof(*ru));
}

static void
rusagePlusMax(struct rusage *ru1,
	      struct rusage *ru2,
	      struct rusage *ru)
{
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

	ru->ru_maxrss = max(ru1->ru_maxrss, ru2->ru_maxrss);
	ru->ru_ixrss = max(ru1->ru_ixrss, ru2->ru_ixrss);
	ru->ru_idrss = max(ru1->ru_idrss, ru2->ru_idrss);
	ru->ru_isrss = max(ru1->ru_isrss, ru2->ru_isrss);
	ru->ru_minflt = ru1->ru_minflt + ru2->ru_minflt;
	ru->ru_majflt = ru1->ru_majflt + ru2->ru_majflt;
	ru->ru_nswap = ru1->ru_nswap + ru2->ru_nswap;
	ru->ru_inblock = ru1->ru_inblock + ru2->ru_inblock;
	ru->ru_oublock = ru1->ru_oublock + ru2->ru_oublock;
	ru->ru_msgsnd = ru1->ru_msgsnd + ru2->ru_msgsnd;
	ru->ru_msgrcv = ru1->ru_msgrcv + ru2->ru_msgrcv;
	ru->ru_nsignals = ru1->ru_nsignals + ru2->ru_nsignals;
	ru->ru_nvcsw = ru1->ru_nvcsw + ru2->ru_nvcsw;
	ru->ru_nivcsw = ru1->ru_nivcsw + ru2->ru_nivcsw;
}

static void
rusageMinusMax (struct rusage *ru1,
		struct rusage *ru2,
		struct rusage *ru)
{
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

	ru->ru_maxrss = max(ru1->ru_maxrss, ru2->ru_maxrss);
	ru->ru_ixrss = max(ru1->ru_ixrss, ru2->ru_ixrss);
	ru->ru_idrss = max(ru1->ru_idrss, ru2->ru_idrss);
	ru->ru_isrss = max(ru1->ru_isrss, ru2->ru_isrss);
	ru->ru_minflt = ru1->ru_minflt - ru2->ru_minflt;
	ru->ru_majflt = ru1->ru_majflt - ru2->ru_majflt;
	ru->ru_nswap = ru1->ru_nswap - ru2->ru_nswap;
	ru->ru_inblock = ru1->ru_inblock - ru2->ru_inblock;
	ru->ru_oublock = ru1->ru_oublock - ru2->ru_oublock;
	ru->ru_msgsnd = ru1->ru_msgsnd - ru2->ru_msgsnd;
	ru->ru_msgrcv = ru1->ru_msgrcv - ru2->ru_msgrcv;
	ru->ru_nsignals = ru1->ru_nsignals - ru2->ru_nsignals;
	ru->ru_nvcsw = ru1->ru_nvcsw - ru2->ru_nvcsw;
	ru->ru_nivcsw = ru1->ru_nivcsw - ru2->ru_nivcsw;
}

static uint
rusageTime(struct rusage *ru)
{
	uint	result;

	result = 0;
	result += 1000 * ru->ru_utime.tv_sec;
	result += 1000 * ru->ru_stime.tv_sec;
	result += ru->ru_utime.tv_usec / 1000;
	result += ru->ru_stime.tv_usec / 1000;
	return result;
}

/* Return time as number of milliseconds. */
static inline uint
currentTime()
{
	struct rusage	ru;

	fixedGetrusage(RUSAGE_SELF, &ru);
	return (rusageTime(&ru));
}

/* ------------------------------------------------- */
/*                   initSignalStack                 */
/* ------------------------------------------------- */

static inline void
initSignalStack(GC_state s)
{
        static stack_t altstack;
	size_t ss_size = roundPage(SIGSTKSZ);
	size_t psize = getpagesize();
	void *ss_sp = ssmmap(2 * ss_size, psize, psize);
	altstack.ss_sp = ss_sp + ss_size;
	altstack.ss_size = ss_size;
	altstack.ss_flags = 0;
	sigaltstack(&altstack, NULL);
}

/* ------------------------------------------------- */
/*                  GC_initCounters                  */
/* ------------------------------------------------- */

static int processor_has_sse2=0;

static void readProcessor() {
#if 0
	int status = system("/bin/cat /proc/cpuinfo | /bin/egrep -q '^flags.*:.* mmx .*xmm'");
  
	if (status==0)
		processor_has_sse2=1;
	else
		processor_has_sse2=0;
#endif
	processor_has_sse2=0;
}

/* ------------------------------------------------- */
/*                    getRAMsize                     */
/* ------------------------------------------------- */

/*
 * Set RAM and SWAP size.  Very Linux specific.
 * Note the total amount of RAM is multiplied by ramSlop so that we don't
 * use all of memory or start swapping.  It used to be .95, but Linux
 * 2.2 is more aggressive about swapping.
 */

static inline void
setMemInfo(GC_state s)
{
	struct sysinfo	sbuf;

	unless (0 == sysinfo(&sbuf))
		diee("sysinfo failed");
	s->totalRam = sbuf.totalram;
	s->totalSwap = sbuf.totalswap;
}

inline void
GC_initCounters(GC_state s)
{
	initSignalStack(s);
	setMemInfo(s);
	s->bytesAllocated = 0;
	s->bytesCopied = 0;
	s->canHandle = 0;
	s->currentThread = BOGUS_THREAD;
	rusageZero(&s->ru_gc);
	s->inSignalHandler = FALSE;
	s->maxPause = 0;
	s->maxHeapSizeSeen = 0;
	s->maxStackSizeSeen = 0;
	s->maxBytesLive = 0;
	s->numGCs = 0;
	s->numLCs = 0;
	s->savedThread = BOGUS_THREAD;
	s->signalHandler = BOGUS_THREAD;
	sigemptyset(&s->signalsHandled);
	s->signalIsPending = FALSE;
	sigemptyset(&s->signalsPending);
	s->startTime = currentTime();
	/* The next bit is for heap resizing. */
	s->minLive = 20;
	s->maxLive = 3;
	/* Set liveRatio (close) to the geometric mean of minLive and maxLive. */
	{ 
		uint i;
		for (i = s->maxLive; i * i <= s->minLive * s->maxLive; ++i)
			/* Nothing */ ;
		s->liveRatio = i;
	}
 	readProcessor();
}

/* ------------------------------------------------- */
/*                 GC_setHeapParams                  */
/* ------------------------------------------------- */

/* set fromSize and maybe maxHeapSize, depending on whether useFixedHeap.
 * size must not be an approximation, because setHeapParams will die if it
 * can't set fromSize big enough.
 */
inline void
GC_setHeapParams(GC_state s, uint size)
{
	if (s->useFixedHeap) {
		if (0 == s->fromSize)
			s->fromSize = roundPage(s->ramSlop * s->totalRam);
	        s->fromSize = roundPage(s->fromSize / 2);
	} else {
		if (0 == s->maxHeapSize) {
			s->maxHeapSize =
				roundPage(s->ramSlop *
						(s->maxHeapSwap
						? s->totalRam + s->totalSwap
						: s->totalRam));
		}
		s->maxHeapSize = roundPage(s->maxHeapSize / 2);
		s->fromSize = computeHeapSize(s, size, s->liveRatio);
	}
	if (size + LIMIT_SLOP > s->fromSize)
		die("Out of memory (setHeapParams).");
}

/* ------------------------------------------------- */
/*                      GC_init                      */
/* ------------------------------------------------- */

inline void GC_init(GC_state s)
{
	int i;

	assert(isWordAligned(sizeof(struct GC_thread)));
	GC_initCounters(s);
	for (i = 0; i < s->numGlobals; ++i)
		s->globals[i] = (pointer)BOGUS_POINTER;
	GC_setHeapParams(s, s->bytesLive + initialThreadBytes(s));
	assert(s->bytesLive + initialThreadBytes(s) + LIMIT_SLOP <= s->fromSize);
	GC_fromSpace(s);
	s->frontier = s->base;
	s->toSize = s->fromSize;
	GC_toSpace(s); /* FIXME: Why does toSpace need to be allocated? */
	switchToThread(s, newThreadOfSize(s, STACK_HEADER, initialStackSize(s)));
	assert(initialThreadBytes(s) == s->frontier - s->base);
	assert(s->frontier + s->bytesLive <= s->limit);
	assert(GC_mutatorInvariant(s));
}

#if METER
int sizes[25600];
#endif

/* ------------------------------------------------- */
/*                      forward                      */
/* ------------------------------------------------- */
/*
 * Forward the object pointed to by *pp.
 * Update *pp to point to the new object. 
 */
static inline void
forward(GC_state s, pointer *pp)
{
	pointer p;
	word header;

	assert(isInFromSpace(s, *pp));
	p = *pp;
	header = GC_getHeader(p);
	if (header != FORWARDED) { /* forward the object */
		uint headerBytes, objectBytes, size, skip;
		uint numPointers, numNonPointers;

		/* Compute the space taken by the header and object body. */
		splitHeader(header, &numPointers, &numNonPointers);
		if (isNormal(header)) { /* Fixed size object. */
			headerBytes = GC_OBJECT_HEADER_SIZE;
			objectBytes = toBytes(numPointers + numNonPointers);
			skip = 0;
		} else if (isStackOrContHeader(header)) { /* Stack. */
			GC_stack stack;

			headerBytes = STACK_HEADER_SIZE;
			/* Resize stacks not being used as continuations. */
			stack = (GC_stack)p;
			if (STACK_HEADER == header) {
				if (4 * stack->used <= stack->reserved)
					stack->reserved = stack->reserved / 2;
				else if (4 * stack->used > 3 * stack->reserved)
					stack->reserved = stack->reserved * 2;
				stack->reserved = 
					wordAlign(max(stack->reserved, 
							stackNeedsReserved(s, stack)));
				if (stack->reserved > s->maxStackSizeSeen)
					s->maxStackSizeSeen = stack->reserved;
				assert(stackTopIsOk(s, stack));
			}
			objectBytes = sizeof (struct GC_stack) + stack->used;
			skip = stack->reserved - stack->used;
		} else { /* Array. */
			headerBytes = GC_ARRAY_HEADER_SIZE;
			objectBytes = extractArrayNumBytes(p, numPointers,
								numNonPointers);
			skip = 0;
			/* Empty arrays have POINTER_SIZE bytes for the 
			 * forwarding pointer.
			 */
			if (0 == objectBytes) objectBytes = POINTER_SIZE;
		} 
		size = headerBytes + objectBytes;
		/* This check is necessary, because toSpace may be smaller
		 * than fromSpace, and so the copy may fail.
		 */
  		if (s->back + size + skip > s->toLimit)
			die("Out of memory (forward).\nsize=%u skip=%u remaining=%d s->fromSize=%d s->toSize=%d headerBytes=%d objectBytes=%d header=%x\nDiagnostic: probably a RAM problem.",
				size, skip, s->toLimit - s->back, s->fromSize, s->toSize, headerBytes, objectBytes, header);
  		/* Copy the object. */
 		if (FALSE and processor_has_sse2 and size >= 8192) {
			extern void bcopy_simd(void *, void const *, int);
			bcopy_simd(p - headerBytes, s->back, size);
 		} else {
			uint	*to,
				*from,
				*limit;

			to = (uint *)s->back;
			from = (uint *)(p - headerBytes);
			assert (isWordAligned((uint)to));
			assert (isWordAligned((uint)from));
			assert (isWordAligned(size));
			limit = (uint *)((char *)from + size);
			until (from == limit)
				*to++ = *from++;
		}
#if METER
		if (size < sizeof(sizes)/sizeof(sizes[0])) sizes[size]++;
#endif
 		/* Store the forwarding pointer in the old object. */
		*(word*)(p - WORD_SIZE) = FORWARDED;
		*(pointer*)p = s->back + headerBytes;
		/* Update the back of the queue. */
		s->back += size + skip;
		assert(isWordAligned((uint)s->back));
	}
	*pp = *(pointer*)p;
	assert(isInToSpace(s, *pp));
}

static inline void forwardEachPointerInRange(GC_state s, pointer front,
						pointer *back) {
	pointer b;

	b = *back;
	assert(front <= b);
	while (front < b) {
		while (front < b) {
			assert(isWordAligned((uint)front));
			front = GC_foreachPointerInObject(s, forward, toData(front));
		}
		b = *back;
	}
	assert(front == *back);
}

/* ------------------------------------------------- */
/*                       doGC                        */
/* ------------------------------------------------- */

static inline ulong meg (uint n) {
	return n / (1024ul * 1024ul);
}

void GC_doGC(GC_state s, uint bytesRequested, uint stackBytesRequested) {
	uint gcTime;
	uint size;
	uint live;
	uint needed;
	int try;
	pointer front;
	struct rusage ru_start, ru_finish, ru_total;

	assert(invariant(s));
	if (DEBUG or s->messages)
		fprintf(stderr, "Starting gc.\n");
	fixedGetrusage(RUSAGE_SELF, &ru_start);
	if (s->useFixedHeap)
		goto toSpaceReady;
	/* Get toSpace ready. */
	live = s->bytesLive + bytesRequested + stackBytesRequested;
	needed = computeHeapSize (s, live, s->liveRatio);
	/* We would like toSpace to be as big as fromSpace. Although, if we are
	 * unable to get space, then we will not insist on this.  See below.
         */
	if (needed < s->fromSize)
		needed = s->fromSize;
	/* Massage toSpace so that it is of needed size. */
	if (s->toBase != NULL) {
		 if (s->toSize < needed) {
			if (DEBUG or s->messages)
				fprintf(stderr, "Unmapping toSpace\n");
			smunmap(s->toBase, s->toSize);
			s->toBase = NULL;
		 } else {
			uint delete;
			assert(s->toSize >= needed);
			delete = s->toSize - needed;
			if (DEBUG or s->messages)
				fprintf(stderr, "Shrinking toSpace by %u\n",
					 delete);
			smunmap(s->toBase + needed, delete);
			s->toSize = needed;
			goto toSpaceReady;
		 }
	}
	/* Allocate toSpace. */
	assert(s->toBase == (void*)NULL);
	try = 0;
	/* Try to allocate toSpace with the initial needed, but if that fails,
	 * then back off to small multiples of live (as specified by ks below).
         * This may produce a toSpace that is smaller than fromSpace.
         */
	loop {
		static double ks[13] = {1.99, 1.99, 1.8, 1.7, 1.6, 1.5, 
					1.4, 1.3, 1.2, 1.15, 1.1, 1.05, 1.01};

		s->toSize = needed;
		toSpace(s);
       		if (s->toBase != (void*)-1) 
			goto toSpaceReady;
		if (13 == try) {
			static char buffer[256];
			assert(13 == try);
			sprintf(buffer, "/bin/cat /proc/%d/maps\n", getpid());
			(void)system(buffer);
			die("Out of swap space: cannot obtain %u bytes.", 
				needed);
		}
		assert(0 <= try and try < 13);
		s->toBase = (void*)NULL;
		needed = roundPage(ks[try] * (double)live);
		try++;
		if (needed >= s->maxHeapSize) 
			continue;
		if (s->messages)
			fprintf(stderr, "[Requested %luMb cannot be satisfied, allocating %luMb instead. Live = %luMb, k=%.2f]\n",
			meg(s->toSize), meg(needed), meg(live), ks[try]);
	}
toSpaceReady:
 	s->numGCs++;
 	s->bytesAllocated += s->frontier - s->base - s->bytesLive;
	s->back = s->toBase;
	if (DEBUG or s->messages) {
		fprintf(stderr, "fromSpace %s", uintToCommaString(s->fromSize));
		fprintf(stderr, "  toSpace %s\n", uintToCommaString(s->toSize));
	}
	s->toLimit = s->toBase + s->toSize;
	/* The actual GC. */
	front = s->back;
	GC_foreachGlobal(s, forward);
	forwardEachPointerInRange(s, front, &s->back);
	size = s->fromSize;
	/* Swap fromSpace and toSpace. */
	{
		pointer tmp;
		tmp = s->base;
		s->base = s->toBase;
		s->toBase = tmp;
	}
	{
		uint tmp;
		tmp = s->fromSize;
		s->fromSize = s->toSize;
		s->toSize = tmp;
	}
	GC_setStack(s);
	s->frontier = s->back;
	s->bytesLive = s->frontier - s->base;
	if (s->bytesLive > s->maxBytesLive)
		s->maxBytesLive = s->bytesLive;
	/* Resize heap, if necessary. */
	unless (s->useFixedHeap) {
		uint needed;

		needed = s->bytesLive + bytesRequested;
		if (computeHeapSize(s, needed, s->minLive) < s->fromSize) {
			/* shrink heap */
			uint keep;

			keep = computeHeapSize(s, needed, s->liveRatio);
			if (DEBUG or s->messages)
				fprintf(stderr, "Shrinking heap to %u bytes.\n", keep);
			assert(keep <= s->fromSize);
			smunmap(s->base + keep, s->fromSize - keep);
			s->fromSize = keep;
		}
	
		if ((s->toSize < s->fromSize)
		       /* For improved memory behavior when swapping, unmap
			* toSpace if total memory used is >totalRam.
			*/
  		    or (s->fromSize + s->toSize > s->ramSlop * s->totalRam)
		    or (computeHeapSize(s, needed, s->maxLive) > s->fromSize)) {
			/* prepare to allocate new toSpace at next GC */
			smunmap(s->toBase, s->toSize);
			s->toBase = NULL;
		} else {
		        /* shrink toSpace so that s->toSize == s->fromSize */
			smunmap(s->toBase + s->fromSize, s->toSize - s->fromSize);
	 		s->toSize = s->fromSize;
		}
	}
	setLimit(s);
	s->bytesCopied += s->bytesLive;
	fixedGetrusage(RUSAGE_SELF, &ru_finish);
	rusageMinusMax(&ru_finish, &ru_start, &ru_total);
	rusagePlusMax(&s->ru_gc, &ru_total, &s->ru_gc);
	gcTime = rusageTime(&ru_total);
	s->maxPause = max(s->maxPause, gcTime);
	if (DEBUG or s->messages) {
		fprintf(stderr, "Finished gc.\n");
		fprintf(stderr, "time(ms): %s\n", intToCommaString(gcTime));
		fprintf(stderr, "live(bytes): %s (%.1f%%)\n", 
			intToCommaString(s->bytesLive),
			100.0 * ((double) s->bytesLive) / size);
	}
	unless (s->frontier + bytesRequested <= s->limit) {
		if (s->useFixedHeap or s->fromSize == s->maxHeapSize) {
			die("Out of memory (doGC).");
		} 
		if (DEBUG)
			fprintf(stderr, "Recursive call to doGC.\n");
		GC_doGC(s, bytesRequested, 0);
	}
	assert(s->frontier + bytesRequested <= s->limit);
	assert(invariant(s));
}

/* ------------------------------------------------- */
/*                       GC_gc                       */
/* ------------------------------------------------- */

void GC_gc(GC_state s, uint bytesRequested, bool force,
		string file, int line) {
	s->currentThread->bytesNeeded = bytesRequested;
	do {
	uint stackBytesRequested;
	GC_enter(s);
	if (DEBUG) {
		fprintf (stderr, "%s %d: ", file, line);
		GC_display(s, stderr);
	}
	stackBytesRequested =
		(stackTopIsOk(s, s->currentThread->stack))
		? 0 
		: stackBytes(2 * s->currentThread->stack->reserved);
	if (DEBUG)
		fprintf(stderr, "bytesRequested = %d  stackBytesRequested = %d\n",
				bytesRequested, stackBytesRequested);
	if (force or (s->frontier + bytesRequested + stackBytesRequested 
			> s->limit)) {
		if (s->messages)
			fprintf(stderr, "%s %d: GC_doGC\n", file, line);
		/* This GC will grow the stack, if necessary. */
		GC_doGC (s, bytesRequested, s->currentThread->stack->reserved);
		assert (s->frontier + bytesRequested <= s->limit);
	} else if (not (stackTopIsOk (s, s->currentThread->stack))) {
		uint size;
		GC_stack stack;

		size = 2 * s->currentThread->stack->reserved;
		if (DEBUG)
			fprintf (stderr, "Growing stack to size %u.\n", size);
		if (size > s->maxStackSizeSeen)
			s->maxStackSizeSeen = size;
		/* The newStack can't cause a GC, because we checked above to 
		 * make sure there was enough space. 
		 */
		stack = newStack(s, STACK_HEADER, size);
		stackCopy(s->currentThread->stack, stack);
		s->currentThread->stack = stack;
		GC_setStack(s);
		assert(s->frontier + bytesRequested <= s->limit);
	} else {
		assert (0 == s->canHandle);
		/* Switch to the signal handler thread. */
		if (DEBUG) {
			fprintf(stderr, "switching to signal handler\n");
			GC_display(s, stderr);
		}
		assert(s->signalIsPending);
		s->signalIsPending = FALSE;
		s->inSignalHandler = TRUE;
		s->savedThread = s->currentThread;
		switchToThread(s, s->signalHandler);
	}
	leave(s);
	} while (s->frontier + s->currentThread->bytesNeeded > s->limit);
}

/* ------------------------------------------------- */
/*                 GC_createStrings                  */
/* ------------------------------------------------- */

void GC_createStrings(GC_state s, struct GC_stringInit inits[]) {
	pointer frontier;
	int i;

	assert(invariant(s));
	frontier = s->frontier;
	for(i = 0; inits[i].str != NULL; ++i) {
		uint numElements, numBytes;

		numElements = inits[i].size;
		numBytes = GC_ARRAY_HEADER_SIZE
			+ ((0 == numElements) 
				? POINTER_SIZE 
				: wordAlign(numElements));
		if (frontier + numBytes >= s->limit)
			die("Unable to allocate string constant \"%s\".", 
				inits[i].str);
		*(word*)frontier = numElements;
		*(word*)(frontier + WORD_SIZE) = STRING_HEADER;
		s->globals[inits[i].globalIndex] = 
			frontier + GC_ARRAY_HEADER_SIZE;
		{
			int j;

			for (j = 0; j < numElements; ++j)
				*(frontier + GC_ARRAY_HEADER_SIZE + j) 
					= inits[i].str[j];
		}
		frontier += numBytes;
	}
	s->frontier = frontier;
	assert(GC_mutatorInvariant(s));
}

/* ------------------------------------------------- */
/*                      GC_done                      */
/* ------------------------------------------------- */

static void displayUint (string name, uint n) {
	fprintf (stderr, "%s: %s\n", name, uintToCommaString(n));
}

static void displayUllong (string name, ullong n) {
	fprintf (stderr, "%s: %s\n", name, ullongToCommaString(n));
}

inline void
GC_done (GC_state s)
{
	GC_enter(s);
	smunmap(s->base, s->fromSize);
	if (s->toBase != NULL) smunmap(s->toBase, s->toSize);
	if (s->summary) {
		double time;
		uint gcTime = rusageTime(&s->ru_gc);

		displayUint("max semispace size(bytes)", s->maxHeapSizeSeen);
		displayUint("max stack size(bytes)", s->maxStackSizeSeen);
		time = (double)(currentTime() - s->startTime);
		fprintf(stderr, "GC time(ms): %s (%.1f%%)\n",
			intToCommaString(gcTime), 
			(0.0 == time) ? 0.0 
			: 100.0 * ((double) gcTime) / time);
		displayUint("maxPause(ms)", s->maxPause);
		displayUint("number of GCs", s->numGCs);
		displayUllong("number of LCs", s->numLCs);
		displayUllong("bytes allocated",
	 			s->bytesAllocated 
				+ (s->frontier - s->base - s->bytesLive));
		displayUllong("bytes copied", s->bytesCopied);
		displayUint("max bytes live", s->maxBytesLive);
#if METER
		{
			int i;
			for(i = 0; i < cardof(sizes); ++i) {
				if (0 != sizes[i])
					fprintf(stderr, "COUNT[%d]=%d\n", i, sizes[i]);
		  	}
		}
#endif

	}	
}

/* GC_handler sets s->limit = 0 so that the next limit check will fail. 
 * Signals need to be blocked during the handler (i.e. it should run atomically)
 * because sigaddset does both a read and a write of s->signalsPending.
 * The signals are blocked by Posix_Signal_handle (see Posix/Signal/Signal.c).
 */
inline void
GC_handler(GC_state s, int signum)
{
	if (DEBUG)
		fprintf(stderr, "GC_handler\n");
	if (0 == s->canHandle) {
		if (DEBUG)
			fprintf(stderr, "setting limit = 0\n");
		s->limit = 0;
	}
	sigaddset(&s->signalsPending, signum);
	s->signalIsPending = TRUE;
}

inline void
GC_finishHandler(GC_state s, GC_thread t)
{
	if (DEBUG) {
		fprintf(stderr, "GC_finishHandler\n");
		GC_display(s, stderr);
	}
	GC_enter(s);
	assert(t != BOGUS_THREAD);
	s->inSignalHandler = FALSE;	
	sigemptyset(&s->signalsPending);
	switchToThread(s, t);
	leave(s);
}

/* ------------------------------------------------- */
/*                   GC_objectSize                   */
/* ------------------------------------------------- */
/* Compute the space taken by the header and object body. */

inline uint
GC_objectSize(pointer p)
{
	uint headerBytes, objectBytes;
       	word header;
	uint numPointers, numNonPointers;

	header = GC_getHeader(p);
	splitHeader(header, &numPointers, &numNonPointers);
	if (isNormal(header)) { /* Fixed size object. */
		headerBytes = GC_OBJECT_HEADER_SIZE;
		objectBytes = toBytes(numPointers + numNonPointers);
	} else if (isStackOrContHeader(header)) { /* Stack. */
		headerBytes = STACK_HEADER_SIZE;
		objectBytes = sizeof(struct GC_stack) + ((GC_stack)p)->reserved;
	} else { /* Array. */
		headerBytes = GC_ARRAY_HEADER_SIZE;
		objectBytes = extractArrayNumBytes(p, numPointers,
							numNonPointers);
		/* Empty arrays have POINTER_SIZE bytes for the 
		 * forwarding pointer.
		 */
		if (0 == objectBytes) objectBytes = POINTER_SIZE;
	}
	return headerBytes + objectBytes;
}
