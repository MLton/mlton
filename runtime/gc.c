/* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */

#include "gc.h"
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <string.h>
#if (defined (__FreeBSD__))
#include <sys/types.h>
#endif
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/times.h>
#include <time.h>
#if (defined (__CYGWIN__))
#include <windows.h>
#endif
#if (defined (__linux__))
#include <values.h>
#endif
#if (defined (__FreeBSD__))
#include <limits.h>
#endif

#define METER FALSE  /* Displays distribution of object sizes at program exit. */

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
	COPY_CHUNK_SIZE = 0x800000,
	DEBUG = FALSE,
	DEBUG_ARRAY = FALSE,
	DEBUG_DETAILED = FALSE,
	DEBUG_GENERATIONAL = FALSE,
	DEBUG_MARK_COMPACT = FALSE,
	DEBUG_MEM = FALSE,
	DEBUG_RESIZING = FALSE,
	DEBUG_SIGNALS = FALSE,
	DEBUG_STACKS = FALSE,
	DEBUG_THREADS = FALSE,
	FORWARDED = 0xFFFFFFFF,
	HEADER_SIZE = WORD_SIZE,
	STACK_HEADER_SIZE = WORD_SIZE,
};

typedef enum {
	MARK_MODE,
	UNMARK_MODE,
} MarkMode;

#define BOGUS_THREAD (GC_thread)BOGUS_POINTER
#define STACK_HEADER GC_objectHeader (STACK_TYPE_INDEX)
#define STRING_HEADER GC_objectHeader (STRING_TYPE_INDEX)
#define THREAD_HEADER GC_objectHeader (THREAD_TYPE_INDEX)
#define WORD8_VECTOR_HEADER GC_objectHeader (WORD8_TYPE_INDEX)

#define SPLIT_HEADER()								\
	do {									\
		int objectTypeIndex;						\
		GC_ObjectType *t;						\
										\
		assert (1 == (header & 1));					\
		objectTypeIndex = (header & TYPE_INDEX_MASK) >> 1;		\
		assert (0 <= objectTypeIndex					\
				and objectTypeIndex < s->maxObjectTypeIndex);	\
		t = &s->objectTypes [objectTypeIndex];				\
		tag = t->tag;							\
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
	default:
	die ("bad tag %u", t);
	}
}

static inline ulong meg (uint n) {
	return n / (1024ul * 1024ul);
}

static inline uint toBytes(uint n) {
	return n << 2;
}

#if (defined (__linux__) || defined (__FreeBSD__))
static inline uint min(uint x, uint y) {
	return ((x < y) ? x : y);
}


static inline uint max (uint x, uint y) {
	return ((x > y) ? x : y);
}
#endif

static inline W64 min64 (W64 x, W64 y) {
	return ((x < y) ? x : y);
}

static inline W64 max64 (W64 x, W64 y) {
	return ((x > y) ? x : y);
}


static inline uint align (uint a, uint b) {
	assert (a >= 0);
	assert (b >= 1);
	a += b - 1;
	a -= a % b;
	return a;	
}

static bool isAligned (uint a, uint b) {
	return 0 == a % b;
}

#ifndef NODEBUG
static bool isPageAligned (GC_state s, size_t size) {
	return isAligned (size, s->pageSize);
}
#endif

#if (defined (__linux__) || defined (__FreeBSD__))
/* A super-safe mmap.
 *  Allocates a region of memory with dead zones at the high and low ends.
 *  Any attempt to touch the dead zone (read or write) will cause a
 *   segmentation fault.
 */
static void *ssmmap (size_t length, size_t dead_low, size_t dead_high) {
	void *base,*low,*result,*high;

	base = smmap (length + dead_low + dead_high);
	low = base;
	if (mprotect (low, dead_low, PROT_NONE))
		diee ("mprotect failed");
	result = low + dead_low;
	high = result + length;
	if (mprotect(high, dead_high, PROT_NONE))
		diee ("mprotect failed");
	return result;
}
#endif

static void release (void *base, size_t length) {
#if (defined (__CYGWIN__))
	if (DEBUG_MEM)
		fprintf (stderr, "VirtualFree (0x%x, 0, MEM_RELEASE)\n", 
				(uint)base);
	if (0 == VirtualFree (base, 0, MEM_RELEASE))
		die ("VirtualFree release failed");
#elif (defined (__linux__) || defined (__FreeBSD__))
	smunmap (base, length);
#endif
}

static void decommit (void *base, size_t length) {
#if (defined (__CYGWIN__))
	if (DEBUG_MEM)
		fprintf (stderr, "VirtualFree (0x%x, %u, MEM_DECOMMIT)\n", 
				(uint)base, (uint)length);
	if (0 == VirtualFree (base, length, MEM_DECOMMIT))
		die ("VirtualFree decommit failed");
#elif (defined (__linux__) || defined (__FreeBSD__))
	smunmap (base, length);
#endif
}

#if (defined (__CYGWIN__))

static void showMaps () {
	MEMORY_BASIC_INFORMATION buf;
	LPCVOID lpAddress;
	char *state = "<unset>";
	char *protect = "<unset>";

	for (lpAddress = 0; lpAddress < (LPCVOID)0x80000000; ) {
		VirtualQuery (lpAddress, &buf, sizeof(buf));

		switch (buf.Protect) {
		case PAGE_READONLY:
			protect = "PAGE_READONLY";
			break;
		case PAGE_READWRITE:
			protect = "PAGE_READWRITE";
			break;
		case PAGE_WRITECOPY:
			protect = "PAGE_WRITECOPY";
			break;
		case PAGE_EXECUTE:
			protect = "PAGE_EXECUTE";
			break;
		case PAGE_EXECUTE_READ:
			protect = "PAGE_EXECUTE_READ";
			break;
		case PAGE_EXECUTE_READWRITE:
			protect = "PAGE_EXECUTE_READWRITE";
			break;
		case PAGE_EXECUTE_WRITECOPY:
			protect = "PAGE_EXECUTE_WRITECOPY";
			break;
		case PAGE_GUARD:
			protect = "PAGE_GUARD";
			break;
		case PAGE_NOACCESS:
			protect = "PAGE_NOACCESS";
			break;
		case PAGE_NOCACHE:
			protect = "PAGE_NOCACHE";
			break;
		}
		switch (buf.State) {
		case MEM_COMMIT:
			state = "MEM_COMMIT";
			break;
		case MEM_FREE:
			state = "MEM_FREE";
			break;
		case MEM_RESERVE:
			state = "MEM_RESERVE";
			break;
		}
		fprintf(stderr, "0x%8x %10u  %s %s\n",
			(uint)buf.BaseAddress,
			(uint)buf.RegionSize,
			state, protect);
		lpAddress += buf.RegionSize;
	}
}

static void showMem () {
	MEMORYSTATUS ms; 

	ms.dwLength = sizeof (MEMORYSTATUS); 
	GlobalMemoryStatus (&ms); 
	fprintf(stderr, "Total Phys. Mem: %ld\nAvail Phys. Mem: %ld\nTotal Page File: %ld\nAvail Page File: %ld\nTotal Virtual: %ld\nAvail Virtual: %ld\n",
			 ms.dwTotalPhys, 
			 ms.dwAvailPhys, 
			 ms.dwTotalPageFile, 
			 ms.dwAvailPageFile, 
			 ms.dwTotalVirtual, 
			 ms.dwAvailVirtual); 
	showMaps();
}

#elif (defined (__linux__))

static void showMem() {
	static char buffer[256];

	sprintf(buffer, "/bin/cat /proc/%d/maps\n", getpid());
	(void)system(buffer);
}

#elif (defined (__FreeBSD__))

static void showMem() {
	static char buffer[256];

	sprintf(buffer, "/bin/cat /proc/%d/map\n", getpid());
	(void)system(buffer);
}

#endif

static inline void copy (pointer src, pointer dst, uint size) {
	uint	*to,
		*from,
		*limit;

	if (DEBUG_DETAILED)
		fprintf (stderr, "copy (0x%08x, 0x%08x, %u)\n",
				(uint)src, (uint)dst, size);
	assert (isWordAligned((uint)src));
	assert (isWordAligned((uint)dst));
	assert (isWordAligned(size));
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

int fixedGetrusage (int who, struct rusage *rup) {
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
		diee ("Impossible: times() failed");
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
		die ("getrusage() accepted unknown who: %d", who);
		exit (1);  /* needed to keep gcc from whining. */
	}
	rup->ru_utime.tv_sec = user / hz;
	rup->ru_utime.tv_usec = (user % hz) * (1000000 / hz);
	rup->ru_stime.tv_sec = sys / hz;
	rup->ru_stime.tv_usec = (sys % hz) * (1000000 / hz);
	return (0);
}

static inline void rusageZero (struct rusage *ru) {
	memset(ru, 0, sizeof(*ru));
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
static inline uint currentTime () {
	struct rusage	ru;

	fixedGetrusage (RUSAGE_SELF, &ru);
	return rusageTime (&ru);
}

static inline void startTiming (struct rusage *ru_start) {
	fixedGetrusage (RUSAGE_SELF, ru_start);
}

static inline uint stopTiming (struct rusage *ru_start, struct rusage *ru_gc) {
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
			(uint) s->heap.cardMap,
   			(uint) s->heap.oldGen,
			uintToCommaString (s->heap.oldGenSize),
			(uint) s->heap.oldGen + s->heap.oldGenSize,
			(uint) s->heap.nursery, 
			(uint) s->frontier,
			s->frontier - s->heap.nursery,
			s->limitPlusSlop - s->frontier);
	fprintf (stream, "\tcanHandle = %d\n", s->canHandle);
	fprintf (stderr, "\tcurrentThread = 0x%08x\n", (uint) s->currentThread);
	fprintf (stream, "\texnStack = %u  bytesNeeded = %u  reserved = %u  used = %u\n",
			s->currentThread->exnStack,
			s->currentThread->bytesNeeded,
			s->currentThread->stack->reserved,
			s->currentThread->stack->used);
	fprintf (stream, "\tstackBottom = 0x%08x\nstackTop - stackBottom = %u\nstackLimit - stackTop = %u\n",
			(uint)s->stackBottom,
			s->stackTop - s->stackBottom,
			(s->stackLimit - s->stackTop));
	if (DEBUG_DETAILED) {
		int i;

		fprintf (stderr, "crossMap trues");
		for (i = 0; i < s->heap.numCards; ++i)
			if (s->heap.crossMap[i])
				fprintf (stderr, " %u", i);
		fprintf (stderr, "\n");
	}		
}

static inline uint cardNumToSize (GC_state s, uint n) {
	return n << s->cardSizeLog2;
}

static inline uint divCardSize (GC_state s, uint n) {
	return n >> s->cardSizeLog2;
}

static inline bool cardIsMarked (GC_state s, pointer p) {
	return s->heap.cardMap[divCardSize (s, (uint)p)];
}

static inline void markCard (GC_state s, pointer p) {
	if (s->mutatorMarksCards)
		s->heap.cardMap[divCardSize (s, (uint)p)] = '\001';
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

static inline uint stackBytes (uint size) {
	uint res;

	res = wordAlign (HEADER_SIZE + sizeof (struct GC_stack) + size);
	if (DEBUG_STACKS)
		fprintf (stderr, "%s = stackBytes (%s)\n",
				uintToCommaString (res),
				uintToCommaString (size));
	return res;
}

static inline pointer stackBottom (GC_stack stack) {
	return ((pointer)stack) + sizeof (struct GC_stack);
}

/* Pointer to the topmost word in use on the stack. */
static inline pointer stackTop (GC_stack stack) {
	return stackBottom (stack) + stack->used;
}

/* The maximum value stackTop may take on. */
static inline pointer stackLimit (GC_state s, GC_stack stack) {
	return stackBottom (stack) + stack->reserved - stackSlop (s);
}

static inline bool stackIsEmpty (GC_stack stack) {
	return 0 == stack->used;
}

static inline GC_frameLayout * getFrameLayout (GC_state s, word returnAddress) {
	GC_frameLayout *layout;
	uint index;

	if (s->native)
		index = *((uint*)(returnAddress - 4));
	else
		index = (uint)returnAddress;
	assert (0 <= index and index <= s->maxFrameIndex);
	layout = &(s->frameLayouts[index]);
	assert (layout->numBytes > 0);
	return layout;
}

static inline uint topFrameSize (GC_state s, GC_stack stack) {
	GC_frameLayout *layout;
	
	assert (not (stackIsEmpty (stack)));
	layout = getFrameLayout (s, *(word*)(stackTop (stack) - WORD_SIZE));
	return layout->numBytes;
}

static inline uint stackNeedsReserved (GC_state s, GC_stack stack) {
	return stack->used + stackSlop (s) - topFrameSize (s, stack);
}

/* stackTopIsOk ensures that when this stack becomes current that 
 * the stackTop is less than the stackLimit.
 */
static inline bool stackTopIsOk (GC_state s, GC_stack stack) {
	return stackTop (stack) 
		       	<= stackLimit (s, stack) 
			+ (stackIsEmpty (stack) ? 0 : topFrameSize (s, stack));
}

#ifndef NODEBUG
static bool hasBytesFree (GC_state s, W32 oldGen, W32 nursery) {
	if (DEBUG_DETAILED)
		fprintf (stderr, "hasBytesFree  oldGen = %s  nursery = %s\n",
				uintToCommaString (oldGen),
				uintToCommaString (nursery));
	return s->heap.oldGenSize + oldGen 
			<= s->heap.size - (s->heap.canMinor ? 2 : 1) * s->heap.nurserySize
		and nursery <= s->limitPlusSlop - s->frontier;
}
#endif

static inline pointer object (GC_state s, uint header, W32 bytesRequested,
				bool allocInOldGen) {
	pointer frontier;
	pointer result;

	if (DEBUG)
		fprintf (stderr, "object (0x%08x, %u, %s)\n",
				header, 
				(uint)bytesRequested,
				boolToString (allocInOldGen));
	assert (isWordAligned (bytesRequested));
	assert (allocInOldGen
			? hasBytesFree (s, bytesRequested, 0)
			: hasBytesFree (s, 0, bytesRequested));
	if (allocInOldGen) {
		frontier = s->heap.oldGen + s->heap.oldGenSize;
		s->heap.oldGenSize += bytesRequested;
	} else {
		if (DEBUG_DETAILED)
			fprintf (stderr, "frontier changed from 0x%08x to 0x%08x\n",
					(uint)s->frontier, 
					(uint)(s->frontier + bytesRequested));
		frontier = s->frontier;
		s->frontier += bytesRequested;
	}
	*(uint*)(frontier) = header;
	result = frontier + HEADER_SIZE;
	return result;
}

static inline GC_stack newStack (GC_state s, uint size, bool allocInOldGen) {
	GC_stack stack;

	if (size > s->maxStackSizeSeen)
		s->maxStackSizeSeen = size;
	stack = (GC_stack) object (s, STACK_HEADER, stackBytes (size),
					allocInOldGen);
	stack->reserved = size;
	stack->used = 0;
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%x = newStack (%u)\n", (uint)stack, size);
	return stack;
}

inline void setStack (GC_state s) {
	GC_stack stack;

	stack = s->currentThread->stack;
	s->stackBottom = stackBottom (stack);
	s->stackTop = stackTop (stack);
	s->stackLimit = stackLimit (s, stack);
	/* We must card mark the stack because it will be updated by the mutator.
	 */
	markCard (s, (pointer)stack);
}

static inline void stackCopy (GC_stack from, GC_stack to) {
	assert (from->used <= to->reserved);
	to->used = from->used;
	if (DEBUG_STACKS)
		fprintf (stderr, "stackCopy from 0x%08x to 0x%08x of length %u\n",
				(uint) stackBottom (from), 
				(uint) stackBottom (to),
				from->used);
	memcpy (stackBottom (to), stackBottom (from), from->used);
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
static inline void foreachGlobal (GC_state s, GC_pointerFun f)
{
	int i;

 	for (i = 0; i < s->numGlobals; ++i) {
		if (DEBUG_DETAILED)
			fprintf (stderr, "foreachGlobal %u\n", i);
		maybeCall (f, s, &s->globals [i]);
	}
	if (DEBUG_DETAILED)
		fprintf (stderr, "foreachGlobal threads\n");
	maybeCall (f, s, (pointer*)&s->currentThread);
	maybeCall (f, s, (pointer*)&s->savedThread);
	maybeCall (f, s, (pointer*)&s->signalHandler);
}

/* The number of bytes in an array, not including the header. */
static inline uint
arrayNumBytes (pointer p, 
		     uint numPointers,
		     uint numNonPointers)
{
	uint numElements, bytesPerElement, result;
	
	numElements = GC_arrayNumElements (p);
	bytesPerElement = numNonPointers + toBytes (numPointers);
	result = wordAlign (numElements * bytesPerElement);
	/* Empty arrays have POINTER_SIZE bytes for the forwarding pointer */
	if (0 == result) 
		result = POINTER_SIZE;
	
	return result;
}

/* ---------------------------------------------------------------- */
/*                      foreachPointerInObject                      */
/* ---------------------------------------------------------------- */
/* foreachPointerInObject (s, f, p) applies f to each pointer in the object
 * pointer to by p.
 * Returns pointer to the end of object, i.e. just past object.
 */

inline pointer foreachPointerInObject (GC_state s, GC_pointerFun f, pointer p) {
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
	} else if (ARRAY_TAG == tag) {
		uint numBytes;
		pointer max;

		assert (ARRAY_TAG == tag);
		assert (0 == GC_arrayNumElements (p)
				? 0 == numPointers
				: TRUE);
		numBytes = arrayNumBytes (p, numPointers, numNonPointers);
		max = p + numBytes;
		if (numPointers == 0) {
			/* There are no pointers, just update p. */
			p = max;
		} else if (numNonPointers == 0) {
			assert (0 < GC_arrayNumElements (p));
		  	/* It's an array with only pointers. */
			for (; p < max; p += POINTER_SIZE)
				maybeCall (f, s, (pointer*)p);
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
	} else {
		GC_stack stack;
		pointer top, bottom;
		int i;
		word returnAddress;
		GC_frameLayout *layout;
		GC_offsets frameOffsets;

		assert (STACK_TAG == tag);
		stack = (GC_stack)p;
		bottom = stackBottom (stack);
		top = stackTop (stack);
		assert(stack->used <= stack->reserved);
		while (top > bottom) {
			/* Invariant: top points just past a "return address". */
			returnAddress = *(word*) (top - WORD_SIZE);
			if (DEBUG)
				fprintf(stderr, 
					"  top = %d  return address = 0x%08x.\n", 
					top - bottom, 
					returnAddress);
			layout = getFrameLayout (s, returnAddress); 
			frameOffsets = layout->offsets;
			top -= layout->numBytes;
			for (i = 0 ; i < frameOffsets[0] ; ++i) {
				if (DEBUG)
					fprintf(stderr, 
						"    offset %u  address %x\n", 
						frameOffsets[i + 1],
						(uint)(*(pointer*)(top + frameOffsets[i + 1])));
				maybeCall(f, s, 
					  (pointer*)
					  (top + frameOffsets[i + 1]));
			}
		}
		assert(top == bottom);
		p += sizeof(struct GC_stack) + stack->reserved;
	}
	return p;
}

/* ---------------------------------------------------------------- */
/*                              toData                              */
/* ---------------------------------------------------------------- */

/* If p points at the beginning of an object, then toData p returns a pointer 
 * to the start of the object data.
 */
static inline pointer toData (pointer p) {
	word header;	

	header = *(word*)p;
	if (0 == header)
		/* Looking at the counter word in an array. */
		return p + GC_ARRAY_HEADER_SIZE;
	else
		/* Looking at a header word. */
		return p + GC_NORMAL_HEADER_SIZE;
}

/* ---------------------------------------------------------------- */
/*                      foreachPointerInRange                       */
/* ---------------------------------------------------------------- */

/* foreachPointerInRange (s, front, back, f)
 * Apply f to each pointer between front and *back, which should be a 
 * contiguous sequence of objects, where front points at the beginning of
 * the first object and *back points just past the end of the last object.
 * f may increase *back (for example, this is done by forward).
 * foreachPointerInRange returns apointer to the end of the last object it
 * visits.
 */

static inline pointer foreachPointerInRange (GC_state s, 
						pointer front, 
						pointer *back,
						GC_pointerFun f) {
	pointer b;

	if (DEBUG_DETAILED)
		fprintf (stderr, "foreachPointerInRange  front = 0x%08x  *back = 0x%08x\n",
				(uint)front, *(uint*)back);
	b = *back;
	assert (front <= b);
 	while (front < b) {
		while (front < b) {
			assert (isWordAligned ((uint)front));
	       		if (DEBUG_DETAILED)
				fprintf (stderr, "front = 0x%08x  *back = 0x%08x\n",
						(uint)front, *(uint*)back);
			front = foreachPointerInObject (s, f, toData (front));
		}
		b = *back;
	}
	return front;
}

/* ---------------------------------------------------------------- */
/*                            invariant                             */
/* ---------------------------------------------------------------- */

static bool ratiosOk (GC_state s) {
	return 1.0 < s->growRatio
			and 1.0 < s->nurseryRatio
			and 1.0 < s->markCompactRatio
			and s->markCompactRatio <= s->copyRatio
			and s->copyRatio <= s->liveRatio;
}

#ifndef NODEBUG

static inline bool isInOldGen (GC_state s, pointer p) {
	return s->heap.oldGen <= p and p < s->heap.oldGen + s->heap.oldGenSize;
}

static inline bool isInNursery (GC_state s, pointer p) {
	return s->heap.nursery <= p and p < s->frontier;
}

static inline bool isInFromSpace (GC_state s, pointer p) {
 	return (isInOldGen (s, p) or isInNursery (s, p));
}

static inline void assertIsInFromSpace (GC_state s, pointer *p) {
#ifndef NODEBUG
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
	/* Heap */
	assert (isAligned (s->heap.size, s->cardSize));
	assert (isAligned ((uint)s->heap.oldGen, s->cardSize));
	assert (isAligned (s->heap.oldGenSize, WORD_SIZE));
	assert (isAligned ((uint)s->heap.nursery, WORD_SIZE));
	assert (isAligned (s->heap.nurserySize, WORD_SIZE));
	assert (isAligned ((uint)s->frontier, WORD_SIZE));
	assert (s->heap.nursery <= s->frontier);
	unless (0 == s->heap.size) {
		assert (isPageAligned (s, s->heap.totalSize));
		assert (s->heap.nursery <= s->frontier);
		assert (s->frontier <= s->limitPlusSlop);
		assert (s->limitPlusSlop == s->heap.nursery + s->heap.nurserySize);
		assert (s->limit == s->limitPlusSlop - LIMIT_SLOP);
		assert (s->heap.oldGenSize 
				+ (s->heap.canMinor ? 2 : 1) * s->heap.nurserySize 
 				<= s->heap.size);
	}
	assert (s->heap2.start == NULL or s->heap.size == s->heap2.size);
	/* Check that all pointers are into from space. */
	foreachGlobal (s, assertIsInFromSpace);
	back = s->heap.oldGen + s->heap.oldGenSize;
	if (DEBUG_DETAILED)
		fprintf (stderr, "Checking old generation.\n");
	foreachPointerInRange (s, s->heap.oldGen, &back, assertIsInFromSpace);
	if (DEBUG_DETAILED)
		fprintf (stderr, "Checking nursery.\n");
	foreachPointerInRange (s, s->heap.nursery, &s->frontier, 
				assertIsInFromSpace);
	/* Current thread. */
	stack = s->currentThread->stack;
	assert (isWordAligned (stack->reserved));
	assert (s->stackBottom == stackBottom (stack));
	assert (s->stackTop == stackTop (stack));
 	assert (s->stackLimit == stackLimit (s, stack));
	assert (stack->used == currentStackUsed (s));
	assert (stack->used < stack->reserved);
 	assert (s->stackBottom <= s->stackTop);
	if (DEBUG)
		fprintf (stderr, "invariant passed\n");
	return TRUE;
}

bool mutatorInvariant (GC_state s) {
	if (DEBUG)
		GC_display (s, stderr);
	assert (stackTopIsOk (s, s->currentThread->stack));
	assert (invariant (s));
	return TRUE;
}
#endif /* #ifndef NODEBUG */

static inline void blockSignals (GC_state s) {
	sigprocmask (SIG_BLOCK, &s->signalsHandled, NULL);
}

static inline void unblockSignals (GC_state s) {
	sigprocmask (SIG_UNBLOCK, &s->signalsHandled, NULL);
}

/* ---------------------------------------------------------------- */
/*                         enter and leave                          */
/* ---------------------------------------------------------------- */

/* enter and leave should be called at the start and end of every GC function
 * that is exported to the outside world.  They make sure that signals are
 * blocked for the duration of the function and check the GC invariant
 * They are a bit tricky because of the case when the runtime system is invoked
 * from within an ML signal handler.
 */
void enter (GC_state s) {
	if (DEBUG)
		fprintf (stderr, "enter\n");
	/* used needs to be set because the mutator has changed s->stackTop. */
	s->currentThread->stack->used = currentStackUsed (s);
	if (DEBUG) 
		GC_display (s, stderr);
	unless (s->inSignalHandler) {
		blockSignals (s);
		if (0 == s->limit)
			s->limit = s->limitPlusSlop - LIMIT_SLOP;
	}
	assert (invariant (s));
	if (DEBUG)
		fprintf (stderr, "enter ok\n");
}

void leave (GC_state s) {
	if (DEBUG)
		fprintf (stderr, "leave\n");
	assert (mutatorInvariant (s));
	if (s->signalIsPending and 0 == s->canHandle)
		s->limit = 0;
	unless (s->inSignalHandler)
		unblockSignals (s);
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

	if (live > s->totalRam + s->totalSwap)
		die ("Out of memory: %s bytes live.", 
				ullongToCommaString (live));
	ratio = (float)s->ram / (float)live;
	if (s->useFixedHeap)
		res = s->fixedHeapSize;
        else if (ratio >= s->liveRatio + s->growRatio) {
		/* Cheney copying fits in RAM with desired liveRatio. */
		res = live * s->liveRatio;
		/* If the heap is currently close in size to what we want, leave
		 * it alone.  Favor growing over shrinking.
		 */
		unless (res >= 1.5 * currentSize 
				or res <= .5 * currentSize)
			res = currentSize;
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
		res = min64 (live * s->markCompactRatio, 
				s->totalRam + s->totalSwap);
		/* If the current heap is bigger than res, the shrinking always
		 * sounds like a good idea.  However, depending on what pages
		 * the VM keeps around, growing could be very expensive, if it
		 * involves paging the entire heap.  Hopefully the copy loop
		 * in growFromSpace will make the right thing happen.
		 */ 
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
	h->totalSize = 0;
}

static inline bool heapIsInit (GC_heap h) {
	return 0 == h->size;
}

static inline void heapRelease (GC_state s, GC_heap h) {
	if (NULL == h->start)
		return;
	if (DEBUG or s->messages)
		fprintf (stderr, "Releasing heap at 0x%08x of size %s.\n", 
				(uint)h->start, 
				uintToCommaString (h->size));
	release (h->start, h->totalSize);
	heapInit (h);
}

static inline void heapShrink (GC_state s, GC_heap h, W32 keep) {
	assert (keep <= h->size);
	keep = align (keep, s->cardSize);
	if (0 == keep)
		heapRelease (s, h);
	else if (keep < h->size) {
		uint remove;

		remove = (uint)h->start + h->totalSize 
				- align ((uint)h->oldGen + keep, s->pageSize);
		assert (isAligned (remove, s->pageSize));
		if (DEBUG or s->messages)
			fprintf (stderr, 
				"Shrinking space at 0x%08x of size %s to %s bytes.\n",
				(uint)h->start, 
				uintToCommaString (h->size),
				uintToCommaString (keep));
		h->size = keep;
		if (remove > 0) {
			decommit (h->start + h->totalSize - remove, remove);
			h->totalSize -= remove;
		}
	}
}

static void setLimit (GC_state s) {
	s->limitPlusSlop = s->heap.nursery + s->heap.nurserySize;
	s->limit = s->limitPlusSlop - LIMIT_SLOP;
}

static inline void setNursery (GC_state s, W32 oldGenBytesRequested,
				W32 nurseryBytesRequested) {
	GC_heap h;

	if (DEBUG_DETAILED)
		fprintf (stderr, "setNursery.  oldGenBytesRequested = %s  frontier = 0x%08x\n",  
				uintToCommaString (oldGenBytesRequested),
				(uint)s->frontier);
	h = &s->heap;
	h->nurserySize = h->size - h->oldGenSize - oldGenBytesRequested;
	assert (isAligned (h->nurserySize, WORD_SIZE));
	if (	/* The mutator marks cards. */
		s->mutatorMarksCards
		/* The live ratio is low enough to make generational GC
		 * worthwhile.
		 */
		and (float)h->size / (float)s->bytesLive 
			<= s->generationalRatio
		/* The nursery is large enough to be worth it. */
		and ((float)(h->size - s->bytesLive) 
			/ (float)h->nurserySize) <= s->nurseryRatio
		/* There is enough space in the nursery. */
		and h->nurserySize >= 2 * nurseryBytesRequested
		) {
		h->canMinor = TRUE;
		h->nurserySize /= 2;
		unless (isAligned (h->nurserySize, WORD_SIZE))
			h->nurserySize -= 2;
	} else {
		if (h->nurserySize < nurseryBytesRequested)
			die ("Out of memory.  Insufficient space in nursery.");
		h->canMinor = FALSE;
	}
	h->nursery = h->oldGen + h->size - h->nurserySize;
	s->frontier = h->nursery;
	setLimit (s);
	assert (isAligned (h->nurserySize, WORD_SIZE));
	assert (isAligned ((uint)h->nursery, WORD_SIZE));
	assert (hasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
}

static inline void heapClearCardMap (GC_heap h) {
	memset (h->start, 0, h->numCards);
}

static inline void heapClearCrossMap (GC_heap h) {
	memset (h->crossMap, 0, h->numCards);
}

/* heapCreate (s, h, need, minSize) allocates a heap of the size necessary to
 * work with need live data, and ensures that at least minSize is available.
 * It returns TRUE if it is able to allocate the space, and returns FALSE if it
 * is unable.  If a reasonable size to space is already there, then heapCreate
 * leaves it.
 */
static inline bool heapCreate (GC_state s, GC_heap h, 
				W32 desiredSize, W32 minSize) {
	W32 backoff;

	if (DEBUG)
		fprintf (stderr, "heapCreate  desired size = %s  min size = %s\n",
				uintToCommaString (desiredSize),
				uintToCommaString (minSize));
	assert (heapIsInit (h));
	if (desiredSize < minSize)
		desiredSize = minSize;
	desiredSize = align (desiredSize, s->cardSize);
	assert (0 == h->size and NULL == h->start);
	backoff = (desiredSize - minSize) / 20;
	if (0 == backoff)
		backoff = 1; /* enough to terminate the loop below */
	backoff = align (backoff, s->cardSize);
	/* mmap toggling back and forth between high and low addresses to
         * decrease the chance of virtual memory fragmentation causing an mmap
	 * to fail.  This is important for large heaps.
	 */
	for (h->size = desiredSize; h->size >= minSize; h->size -= backoff) {
		uint cardMapSpace;
		static int direction = 1;
		int i;

		assert (isAligned (h->size, s->cardSize));
		if (s->mutatorMarksCards)
			h->numCards = divCardSize (s, h->size);
		else
			h->numCards = 0;
		if (DEBUG_DETAILED)
			fprintf (stderr, "numCards = %u\n", h->numCards);
		/* We make sure that the card map and cross map take up a 
		 * multiple of s->cardSize bytes so that oldGen starts on a card
		 * boundary.
		 */
		cardMapSpace = align (2 * h->numCards, s->cardSize);
		h->totalSize = align (h->size + cardMapSpace, s->pageSize);
		for (i = 0; i < 32; i++) {
			unsigned long address;

			address = i * 0x08000000ul;
			if (direction)
				address = 0xf8000000ul - address;
#if (defined (__CYGWIN__))
			address = 0; /* FIXME */
			i = 31; /* FIXME */
			h->start = VirtualAlloc ((LPVOID)address, h->totalSize,
							MEM_COMMIT,
							PAGE_READWRITE);
			if (DEBUG_MEM)
				fprintf (stderr, "0x%08x = VirtualAlloc (0x%08x, %u, MEM_COMMIT, PAGE_READWRITE)\n",
						(uint)h->start, 
						(uint)address,
						(uint)h->totalSize);
#elif (defined (__linux__) || defined (__FreeBSD__))
			h->start = mmap (address+(void*)0, h->totalSize, 
						PROT_READ | PROT_WRITE,
						MAP_PRIVATE | MAP_ANON, -1, 0);
			if ((void*)-1 == h->start)
				h->start = (void*)NULL;
#endif
			unless ((void*)NULL == h->start) {
				direction = (direction==0);
				assert (isPageAligned (s, h->totalSize));
				if (h->totalSize > s->maxHeapSizeSeen)
					s->maxHeapSizeSeen = h->totalSize;
				h->oldGen = h->start + cardMapSpace;
				assert (isAligned ((uint)h->oldGen, s->cardSize));
				if (s->mutatorMarksCards) {
					assert (divCardSize (s, (uint)h->oldGen) <= (uint)h->start);
					h->cardMap = h->start 
						- divCardSize (s, (uint)h->oldGen);
					assert (h->start == &h->cardMap[divCardSize(s, (uint)h->oldGen)]);
					assert (h->start + h->numCards - 1 == &h->cardMap[divCardSize (s, (uint)h->oldGen + h->size - WORD_SIZE)]);
					h->crossMap = h->start + h->numCards;
				} else {
					h->cardMap = NULL;
					h->crossMap = NULL;
				}
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
				meg (h->totalSize), meg (backoff), meg (minSize));
	}
	h->totalSize = 0;
	h->size = 0;
	return FALSE;
}

static inline void setCrossMap (GC_state s, pointer p) {
	if (s->mutatorMarksCards and isAligned ((uint)p, s->cardSize)) {
		GC_heap h;	

		h = s->crossMapHeap;
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "crossMap[%u] = TRUE\n",
					divCardSize (s, p - h->oldGen));
		h->crossMap[divCardSize (s, p - h->oldGen)] = '\001';
	}
}

static inline uint objectSize (GC_state s, pointer p)
{
	uint headerBytes, objectBytes;
       	word header;
	uint tag, numPointers, numNonPointers;

	header = GC_getHeader(p);
	SPLIT_HEADER();
	if (NORMAL_TAG == tag) { /* Fixed size object. */
		headerBytes = GC_NORMAL_HEADER_SIZE;
		objectBytes = toBytes (numPointers + numNonPointers);
	} else if (STACK_TAG == tag) { /* Stack. */
		headerBytes = STACK_HEADER_SIZE;
		objectBytes = sizeof(struct GC_stack) + ((GC_stack)p)->reserved;
	} else { /* Array. */
		assert(ARRAY_TAG == tag);
		headerBytes = GC_ARRAY_HEADER_SIZE;
		objectBytes = arrayNumBytes(p, numPointers, numNonPointers);
	}
	return headerBytes + objectBytes;
}

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

#if METER
int sizes[25600];
#endif

/* forward (s, pp) forwards the object pointed to by *pp and updates *pp to 
 * point to the new object. 
 * It also updates the crossMap if the object starts a card boundary.
 */
static inline void forward (GC_state s, pointer *pp) {
	pointer p;
	word header;
	word tag;

	if (DEBUG_DETAILED)
		fprintf(stderr, "forward  pp = 0x%x  *pp = 0x%x\n", (uint)pp, *(uint*)pp);
	assert (isInFromSpace (s, *pp));
	p = *pp;
	header = GC_getHeader(p);
	if (header != FORWARDED) { /* forward the object */
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
			objectBytes = arrayNumBytes (p, numPointers,
								numNonPointers);
			skip = 0;
		} else { /* Stack. */
			GC_stack stack;

			assert (STACK_TAG == tag);
			headerBytes = STACK_HEADER_SIZE;
			/* Shrink stacks that don't use a lot of their reserved
		 	 * space.
			 */
			stack = (GC_stack)p;
			if (stack->used <= stack->reserved / 4) {
				W32 new;

				new = wordAlign (max (stack->reserved / 2, 
							stackNeedsReserved (s, stack)));
				/* It's possible that new > stack->reserved if
				 * the stack is the current one and the stack
				 * top isn't OK.  In that case, we want to leave
				 * the stack alone, because some other part of
				 * the gc will grow the stack.  We cannot do any
				 * growing here because we may run out of to
				 * space.
				 */
				if (new <= stack->reserved)
					stack->reserved = new;
			}
			objectBytes = sizeof (struct GC_stack) + stack->used;
			skip = stack->reserved - stack->used;
		}
		size = headerBytes + objectBytes;
		assert (s->back + size + skip <= s->toLimit);
  		/* Copy the object. */
		if (DEBUG_DETAILED)
			fprintf (stderr, "copying from 0x%08x to 0x%08x of size %u\n",
					(uint)p, (uint)s->back, size);
		copy (p - headerBytes, s->back, size);
#if METER
		if (size < sizeof(sizes)/sizeof(sizes[0])) sizes[size]++;
#endif
 		/* Store the forwarding pointer in the old object. */
		*(word*)(p - WORD_SIZE) = FORWARDED;
		*(pointer*)p = s->back + headerBytes;
		setCrossMap (s, s->back);
		/* Update the back of the queue. */
		s->back += size + skip;
		assert (isAligned ((uint)s->back, WORD_SIZE));
	}
	*pp = *(pointer*)p;
	assert (isInToSpace (s, *pp));
}

static void swapSemis (GC_state s) {
	struct GC_heap h;

	h = s->heap2;
	s->heap2 = s->heap;
	s->heap = h;
	setLimit (s);
}

static inline void cheneyCopy (GC_state s) {
	struct rusage ru_start;

	assert (s->heap2.size >= s->heap.oldGenSize);
	startTiming (&ru_start);		
	s->numCopyingGCs++;
	s->crossMapHeap = &s->heap2;
	s->toSpace = s->heap2.oldGen;
	s->toLimit = s->heap2.oldGen + s->heap2.size;
 	if (DEBUG or s->messages) {
		fprintf (stderr, "Major copying GC.\n");
	 	fprintf (stderr, "fromSpace = 0x%08x of size %s\n", 
				(uint) s->heap.oldGen,
				uintToCommaString (s->heap.size));
		fprintf (stderr, "toSpace = 0x%08x of size %s\n",
				(uint) s->heap2.oldGen,
				uintToCommaString (s->heap2.size));
	}
	assert (s->heap2.start != (void*)NULL);
	/* The next assert ensures there is enough space for the copy to succeed.
	 * It does not say  assert (s->heap2.size >= s->heap.size)
  	 * because that is too strong.
	 */
	assert (s->heap2.size >= s->frontier - s->heap.nursery);
	heapClearCardMap (&s->heap2);
	heapClearCrossMap (&s->heap2);
	s->back = s->heap2.oldGen;
	foreachGlobal (s, forward);
	foreachPointerInRange (s, s->heap2.oldGen, &s->back, forward);
	s->bytesLive = s->back - s->heap2.oldGen;
	s->heap2.oldGenSize = s->bytesLive;
	if (DEBUG)
		fprintf (stderr, "%s bytes live.\n", 
				uintToCommaString (s->bytesLive));
	swapSemis (s);
	s->bytesCopied += s->bytesLive;
	stopTiming (&ru_start, &s->ru_gcCopy);		
 	if (DEBUG or s->messages)
		fprintf (stderr, "Major copying GC done.\n");
}

/* ---------------------------------------------------------------- */
/*                     Minor copying collection                     */
/* ---------------------------------------------------------------- */

static inline void forwardIfInNursery (GC_state s, pointer *pp) {
	pointer p;

	p = *pp;
	if (p < s->heap.nursery)
		return;
	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "intergenerational pointer from 0x%08x to 0x%08x\n",
			(uint)pp, *(uint*)pp);
	assert (s->heap.nursery <= p 
			and p < s->heap.nursery + s->heap.nurserySize);
	forward (s, pp);
}

/* Walk through all the cards and forward all intergenerational pointers. */
static inline void forwardInterGenerationalPointers (GC_state s) {
	pointer cardMap;
	uint cardNum;
	pointer crossMap;
	GC_heap h;
	uint numCards;
	pointer objectStart;
	pointer oldGenStart;
	pointer oldGenEnd;

	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "Forwarding inter-generational pointers.\n");
	h = &s->heap;
	/* Constants. */
	cardMap = h->start;
	crossMap = h->crossMap;
	numCards = divCardSize (s, align (h->oldGenSize, s->cardSize));
	oldGenStart = h->oldGen;
	oldGenEnd = oldGenStart + h->oldGenSize;
	/* Loop variables*/
	objectStart = h->oldGen;
	cardNum = 0;
checkAll:
	assert (cardNum <= numCards);
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
		pointer cardStart;
		pointer cardEnd;
		pointer orig;
		uint size;

		s->markedCards++;
		if (DEBUG_GENERATIONAL)
			fprintf (stderr, "card %u is marked  objectStart = 0x%08x\n", 
					cardNum, (uint)objectStart);
		cardStart = oldGenStart + cardNumToSize (s, cardNum);
		orig = objectStart;
skipObjects:
		size = objectSize (s, toData (objectStart));
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
		objectStart = 
			foreachPointerInRange (s, objectStart, &cardEnd,
						forwardIfInNursery);
		s->minorBytesScanned += objectStart - orig;
		if (objectStart == oldGenEnd)
			goto done;
		cardNum = divCardSize (s, objectStart - oldGenStart);
		goto checkCard;
	} else {
		cardNum++;
		if (crossMap[cardNum]) {
			objectStart = oldGenStart + cardNumToSize (s, cardNum);
			if (DEBUG_GENERATIONAL)
				fprintf (stderr, "crossMap[%u] == TRUE   objectStart = 0x%08x\n", 
						cardNum, (uint)objectStart);
		}
		goto checkAll;
	}
	assert (FALSE);
done:
	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "Forwarding inter-generational pointers done.\n");
}

static inline void minorGC (GC_state s) {
	W32 bytesAllocated;
	W32 bytesCopied;
	struct rusage ru_start;

	if (DEBUG_GENERATIONAL)
		fprintf (stderr, "minorGC  frontier = 0x%08x\n", 
				(uint)s->frontier);
	assert (invariant (s));
	bytesAllocated = s->frontier - s->heap.nursery;
	if (bytesAllocated == 0)
		return;
 	s->bytesAllocated += bytesAllocated;
	if (not s->heap.canMinor) {
		s->heap.oldGenSize += bytesAllocated;
		bytesCopied = 0;
	} else {
		if (DEBUG_GENERATIONAL or s->messages)
			fprintf (stderr, "Minor GC.\n");
		startTiming (&ru_start);
		s->toSpace = s->heap.oldGen + s->heap.oldGenSize;
		s->toLimit = s->toSpace + s->heap.nurserySize;
		assert (invariant (s));
		s->numMinorGCs++;
		s->numMinorsSinceLastMajor++;
		s->crossMapHeap = &s->heap;
		s->back = s->toSpace;
		/* Forward all globals.  Would like to avoid doing this once all
	 	 * the globals have been assigned.
		 */
		foreachGlobal (s, forwardIfInNursery);
		forwardInterGenerationalPointers (s);
		heapClearCardMap (&s->heap);
		foreachPointerInRange (s, s->toSpace, &s->back,
					forwardIfInNursery);
		bytesCopied = s->back - s->toSpace;
		s->bytesCopiedMinor += bytesCopied;
		s->heap.oldGenSize += bytesCopied;
		stopTiming (&ru_start, &s->ru_gcMinor);
		if (DEBUG_GENERATIONAL or s->messages)
			fprintf (stderr, "Minor GC done.  %s bytes copied.\n",
					uintToCommaString (bytesCopied));
	}
	setNursery (s, 0, 0);
	setStack (s);
	assert (invariant (s));
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
	return (((MARK_MODE == m) and isMarked (p))
		or ((UNMARK_MODE == m) and not isMarked (p)));
}

/* mark (s, p) sets all the mark bits in the object graph pointed to by p. 
 * If the mode is MARK, it sets the bits to 1.
 * If the mode is UNMARK, it sets the bits to 0.
 * It returns the amount marked.
 */
W32 mark (GC_state s, pointer root, MarkMode mode) {
	pointer cur;  /* The current object being marked. */
	GC_offsets frameOffsets;
	Header* headerp;
	Header header;
	uint index;
	GC_frameLayout *layout;
	pointer max; /* The end of the pointers in an object. */
	pointer next; /* The next object to mark. */
	Header *nextHeaderp;
	Header nextHeader;
	W32 numBytes;
	uint numNonPointers;
	uint numPointers;
	pointer prev; /* The previous object on the mark stack. */
	W32 size;
	uint tag;
	pointer todo; /* A pointer to the pointer in cur to next. */
	pointer top; /* The top of the next stack frame to mark. */

	if (modeEqMark (mode, root))
		/* Object has already been marked. */
		return 0;
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
	 * todo is a pointer to the pointer inside cur that points to next.
	 * headerp points to the header of next.
	 * header is the header of next.
	 */
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "markNext  cur = 0x%08x  next = 0x%08x  prev = 0x%08x  todo = 0x%08x\n",
				(uint)cur, (uint)next, (uint)prev, (uint)todo);
	assert (not modeEqMark (mode, next));
	assert (header == GC_getHeader (next));
	assert (headerp == GC_getHeaderp (next));
	assert (*(pointer*) todo == next);
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
	header = (MARK_MODE == mode)
			? header | MARK_MASK
			: header & ~MARK_MASK;
	SPLIT_HEADER();
	if (NORMAL_TAG == tag) {
		todo = cur + toBytes (numNonPointers);
		max = todo + toBytes (numPointers);
		size += GC_NORMAL_HEADER_SIZE + (max - cur);
		index = 0;
markInNormal:
		assert (todo <= max);
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInNormal  index = %d\n", index);
		if (todo == max) {
			*headerp = header & ~COUNTER_MASK;
			goto ret;
		}
		next = *(pointer*)todo;
		if (not GC_isPointer (next)) {
markNextInNormal:
			todo += POINTER_SIZE;
			index++;
			goto markInNormal;
		}
		nextHeaderp = GC_getHeaderp (next);
		nextHeader = *nextHeaderp;
		if ((nextHeader & MARK_MASK)
			== (MARK_MODE == mode ? MARK_MASK : 0))
			goto markNextInNormal;
		*headerp = (header & ~COUNTER_MASK) |
				(index << COUNTER_SHIFT);
		headerp = nextHeaderp;
		header = nextHeader;
		goto markNext;
	} else if (ARRAY_TAG == tag) {
		assert (0 == GC_arrayNumElements (cur)
				? 0 == numPointers
				: TRUE);
		numBytes = arrayNumBytes (cur, numPointers, numNonPointers);
		size += GC_ARRAY_HEADER_SIZE + numBytes;
		*headerp = header;
		if (0 == numBytes or 0 == numPointers)
			goto ret;
		assert (0 == numNonPointers);
		max = cur + numBytes;
		todo = cur;
		index = 0;
markInArray:
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInArray index = %d\n", index);
		if (todo == max) {
			*arrayCounterp (cur) = 0;
			goto ret;
		}
		next = *(pointer*)todo;
		if (not GC_isPointer (next)) {
markNextInArray:
			todo += POINTER_SIZE;
			index++;
			goto markInArray;
		}
		nextHeaderp = GC_getHeaderp (next);
		nextHeader = *nextHeaderp;
		if ((nextHeader & MARK_MASK)
			== (MARK_MODE == mode ? MARK_MASK : 0))
			goto markNextInArray;
		*arrayCounterp (cur) = index;
		headerp = nextHeaderp;
		header = nextHeader;
		goto markNext;
	} else {
		assert (STACK_TAG == tag);
		*headerp = header;
		size += stackBytes (((GC_stack)cur)->reserved);
		top = stackTop ((GC_stack)cur);
		assert (((GC_stack)cur)->used <= ((GC_stack)cur)->reserved);
markInStack:
		/* Invariant: top points just past the return address of the
		 * frame to be marked.
		 */
		assert (stackBottom ((GC_stack)cur) <= top);
		if (DEBUG_MARK_COMPACT)
			fprintf (stderr, "markInStack  top = %d\n",
					top - stackBottom ((GC_stack)cur));
					
		if (top == stackBottom ((GC_stack)(cur)))
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
		if ((nextHeader & MARK_MASK)
			== (MARK_MODE == mode ? MARK_MASK : 0)) {
			index++;
			goto markInFrame;
		}
		((GC_stack)cur)->markIndex = index;		
		headerp = nextHeaderp;
		header = nextHeader;
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
	headerp = GC_getHeaderp (prev);
	header = *headerp;
	SPLIT_HEADER();
	if (NORMAL_TAG == tag) {
		todo = prev + toBytes (numNonPointers);
		max = todo + toBytes (numPointers);
		index = (header & COUNTER_MASK) >> COUNTER_SHIFT;
		todo += index * POINTER_SIZE;
		next = cur;
		cur = prev;
		prev = *(pointer*)todo;
		*(pointer*)todo = next;
		todo += POINTER_SIZE;
		index++;
		goto markInNormal;
	} else if (ARRAY_TAG == tag) {
		max = prev + arrayNumBytes (prev, numPointers, numNonPointers);
		index = arrayCounter (prev);
		todo = prev + index * POINTER_SIZE;
		next = cur;
		cur = prev;
		prev = *(pointer*)todo;
		*(pointer*)todo = next;
		todo += POINTER_SIZE;
		index++;
		goto markInArray;
	} else {
		assert (STACK_TAG == tag);
		next = cur;
		cur = prev;
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

/* ---------------------------------------------------------------- */
/*                 Jonkers Mark-compact Collection                  */
/* ---------------------------------------------------------------- */

static inline void markGlobal (GC_state s, pointer *pp) {
	mark (s, *pp, MARK_MODE);
}

static inline void unmarkGlobal (GC_state s, pointer *pp) {
       	mark (s, *pp, UNMARK_MODE);
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

static inline void updateForwardPointers (GC_state s) {
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
	back = s->heap.oldGen + s->heap.oldGenSize;
	front = s->heap.oldGen;
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
			foreachPointerInObject (s, threadInternal, p);
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

static inline void updateBackwardPointersAndSlide (GC_state s) {
	pointer back;
	pointer front;
	uint gap;
	Header header;
	pointer p;
	uint size;

	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "Update backward pointers and slide.\n");
	back = s->heap.oldGen + s->heap.oldGenSize;
	front = s->heap.oldGen;
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
			setCrossMap (s, front - gap);
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
		 * forward pointers to it.  Then unmark it.
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
	s->bytesLive = front - gap - s->heap.oldGen;
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "bytesLive = %u\n", s->bytesLive);
	return;
}

static inline void markCompact (GC_state s) {
	struct rusage ru_start;

	if (DEBUG or s->messages)
		fprintf (stderr, "Major mark-compact GC.\n");
	startTiming (&ru_start);		
	s->numMarkCompactGCs++;
	s->crossMapHeap = &s->heap;
	heapClearCardMap (&s->heap);
	heapClearCrossMap (&s->heap);
	foreachGlobal (s, markGlobal);
	foreachGlobal (s, threadInternal);
	updateForwardPointers (s);
	updateBackwardPointersAndSlide (s);
	s->bytesMarkCompacted += s->bytesLive;
	s->heap.oldGenSize = s->bytesLive;
	stopTiming (&ru_start, &s->ru_gcMarkCompact);
	if (DEBUG or s->messages)
		fprintf (stderr, "Major mark-compact GC done.\n");
}

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
	foreachPointerInRange (s, to, &limit, translatePointer);
}

/* ---------------------------------------------------------------- */
/*                             heapGrow                             */
/* ---------------------------------------------------------------- */

static inline void heapGrow (GC_state s, GC_heap h, W32 desired, W32 minSize) {
	struct GC_heap h2;
	pointer old;
	uint size;

	assert (desired >= h->size);
	if (DEBUG_RESIZING)
		fprintf (stderr, "Growing heap at 0x%08x of size %s to %s bytes.\n",
				(uint)h->start,
				uintToCommaString (h->size),
       				uintToCommaString (desired));
	old = h->oldGen;
	size = h->oldGenSize;
	assert (size <= h->size);
	heapShrink (s, h, size);
	heapInit (&h2);
	/* Allocate a space of the desired size. */
	if (heapCreate (s, &h2, desired, minSize)) {
		pointer from;
		pointer to;

		from = old + size;
		to = h2.oldGen + size;
copy:			
		from -= COPY_CHUNK_SIZE;
		to -= COPY_CHUNK_SIZE;
		if (from > old) {
			copy (from, to, COPY_CHUNK_SIZE);
			heapShrink (s, h, from - old);
			goto copy;
		}
		copy (old, h2.oldGen, from + COPY_CHUNK_SIZE - old);
		heapRelease (s, h);
		*h = h2;
	} else {
		/* Write the heap to a file and try again. */
		FILE *stream;
		char template[80] = "/tmp/FromSpaceXXXXXX";
		int fd;

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
			sfread (h->oldGen, 1, size, stream);
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
	h->oldGenSize = size;
	translateHeap (s, old, h->oldGen, size);
}

/* ---------------------------------------------------------------- */
/*                            resizeHeap                            */
/* ---------------------------------------------------------------- */
/* Resize from space and to space, guaranteeing that at least 'need' bytes are
 * available in from space and that to space is either the same size as from
 * space or is unmapped.
 */
static inline void resizeHeap (GC_state s, W64 need) {
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
		heapGrow (s, &s->heap, desired, need);
	}
	setNursery (s, 0, 0);
	setStack (s);
	/* Resize to space. */
	if (0 == s->heap2.size)
		/* nothing */ ;
	else if (/* toSpace is smaller than fromSpace, so we won't be
             	  * able to use it for the next GC anyways. 
 		  */
 		s->heap2.size < s->heap.size
		or /* Holding on to toSpace may cause paging. */
		s->heap.size + s->heap2.size > s->ram)
		heapRelease (s, &s->heap2);
	else
		heapShrink (s, &s->heap2, s->heap.size);
	assert (s->heap.size >= need);
	assert (invariant (s));
}

static void growStack (GC_state s) {
	uint size;
	GC_stack stack;

	size = 2 * s->currentThread->stack->reserved;
	if (DEBUG_STACKS or s->messages)
		fprintf (stderr, "Growing stack to size %s.\n",
				uintToCommaString (stackBytes (size)));
	assert (hasBytesFree (s, stackBytes (size), 0));
	stack = newStack (s, size, TRUE);
	stackCopy (s->currentThread->stack, stack);
	s->currentThread->stack = stack;
	markCard (s, (pointer)s->currentThread);
	setStack (s);
}

/* ---------------------------------------------------------------- */
/*                        Garbage Collection                        */
/* ---------------------------------------------------------------- */

static inline void majorGC (GC_state s, W32 bytesRequested) {
	s->numMinorsSinceLastMajor = 0;
        if (not s->useFixedHeap
 		and s->heap.size < s->ram
		and (not heapIsInit (&s->heap2)
			or heapCreate (s, &s->heap2,
					heapDesiredSize (s, (W64)s->bytesLive + bytesRequested, 0),
					s->heap.oldGenSize)))
		cheneyCopy (s);
	else
		markCompact (s);
	setNursery (s, 0, 0);
	setStack (s);
	if (s->bytesLive > s->maxBytesLive)
		s->maxBytesLive = s->bytesLive;
	/* Notice that the s->bytesLive below is different than the s->bytesLive
	 * used is an argument to heapCreate above.  Above, it was an estimate.
	 * Here, it is exactly how much was live after the GC.
	 */
	resizeHeap (s, (W64)s->bytesLive + bytesRequested);
	assert (s->heap.oldGenSize + bytesRequested <= s->heap.size);
}

void doGC (GC_state s, 
		W32 oldGenBytesRequested,
		W32 nurseryBytesRequested, 
		bool forceMajor) {
	uint gcTime;
	bool stackTopOk;
	W64 stackBytesRequested;
	struct rusage ru_start;
	W64 totalBytesRequested;
	
	if (DEBUG or s->messages)
		fprintf (stderr, "Starting gc.  Request %s nursery bytes and %s old gen bytes.\n",
				uintToCommaString (nurseryBytesRequested),
				uintToCommaString (oldGenBytesRequested));
	assert (invariant (s));
	startTiming (&ru_start);
	minorGC (s);
	stackTopOk = stackTopIsOk (s, s->currentThread->stack);
	stackBytesRequested =
		stackTopOk
		? 0 
		: stackBytes (2 * s->currentThread->stack->reserved);
	totalBytesRequested = 
		(W64)oldGenBytesRequested 
		+ stackBytesRequested
		+ nurseryBytesRequested;
	if (forceMajor 
		or totalBytesRequested > s->heap.size - s->heap.oldGenSize)
		majorGC (s, totalBytesRequested);
	setNursery (s, oldGenBytesRequested + stackBytesRequested,
			nurseryBytesRequested);
	assert (hasBytesFree (s, oldGenBytesRequested + stackBytesRequested,
					nurseryBytesRequested));
	unless (stackTopOk)
		growStack (s);
	gcTime = stopTiming (&ru_start, &s->ru_gc);
	s->maxPause = max (s->maxPause, gcTime);
	if (DEBUG or s->messages) {
		fprintf (stderr, "Finished gc.\n");
		fprintf (stderr, "time: %s ms\n", intToCommaString (gcTime));
		fprintf (stderr, "old gen size: %s bytes (%.1f%%)\n", 
				intToCommaString (s->heap.oldGenSize),
				100.0 * ((double) s->heap.oldGenSize) 
					/ s->heap.size);
	}
	if (DEBUG) 
		GC_display (s, stderr);
	assert (hasBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
	assert (invariant (s));
}

/* ensureFree (s, b) ensures that upon return
 *      b <= s->limitPlusSlop - s->frontier
 */
static inline void ensureFree (GC_state s, uint b) {
	assert (s->frontier <= s->limitPlusSlop);
	if (b > s->limitPlusSlop - s->frontier)
		doGC (s, 0, b, FALSE);
	assert (b <= s->limitPlusSlop - s->frontier);
}

static inline void switchToThread (GC_state s, GC_thread t) {
	if (DEBUG_THREADS)
		fprintf (stderr, "switchToThread (0x%08x)  used = %u  reserved = %u\n", 
				(uint)t, t->stack->used, t->stack->reserved);
	assert (stackTopIsOk (s, t->stack));
	s->currentThread = t;
	setStack (s);
	ensureFree (s, t->bytesNeeded);
	/* Can not refer to t, because ensureFree may have GC'ed. */
	assert (s->currentThread->bytesNeeded <= s->limitPlusSlop - s->frontier);
}

void GC_switchToThread (GC_state s, GC_thread t) {
	if (DEBUG_THREADS)
		fprintf (stderr, "GC_switchToThread (0x%08x)\n", (uint)t);
	if (FALSE) {
		/* This branch is slower than the else branch, especially 
		 * when debugging is turned on, because it does an invariant
		 * check on every thread switch.
		 * So, we'll stick with the else branch for now.
		 */
	 	enter (s);
	  	switchToThread (s, t);
	 	leave (s);
	} else {
		s->currentThread->stack->used = currentStackUsed (s);
		s->currentThread = t;
		setStack (s);
		if (t->bytesNeeded > s->limitPlusSlop - s->frontier)  {
			enter (s);
			doGC (s, 0, t->bytesNeeded, FALSE);
			leave (s);
		}
	}
	/* Can not refer to t, because we may have GC'ed. */
	assert (s->currentThread->bytesNeeded <= s->limitPlusSlop - s->frontier);
}

static void startHandler (GC_state s) {
	/* Switch to the signal handler thread. */
	if (DEBUG_SIGNALS) {
		fprintf (stderr, "switching to signal handler\n");
		GC_display (s, stderr);
	}
	assert (0 == s->canHandle);
	assert (s->signalIsPending);
	s->signalIsPending = FALSE;
	s->inSignalHandler = TRUE;
	s->savedThread = s->currentThread;
	/* Set s->canHandle to 2, which will be decremented to 1
	 * when swithching to the signal handler thread, which will then
 	 * run atomically and will finish by switching to the thread
	 * to continue with, which will decrement s->canHandle to 0.
 	 */
	s->canHandle = 2;
}


/* GC_startHandler does not do an enter()/leave(), even though it is exported.
 * The basis library uses it as via _ffi, not _prim, and so does not treat it
 * as a runtime call -- so the invariant in enter would fail miserably. 
 * It simulates the relevant part of enter() by blocking signals and resetting
 * the limit.  The leave() wouldn't do anything upon exit  because we are in a
 * signal handler.
 */
void GC_startHandler (GC_state s) {
	blockSignals (s);
	if (0 == s->limit)
		s->limit = s->limitPlusSlop - LIMIT_SLOP;
	startHandler (s);
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
	if (force 
		or bytesRequested > s->limitPlusSlop - s->frontier
		or not (stackTopIsOk (s, s->currentThread->stack))) {
		/* This GC will grow the stack, if necessary. */
		doGC (s, 0, bytesRequested, force);
	} else {
		startHandler (s);
		switchToThread (s, s->signalHandler);
	}
	assert (s->currentThread->bytesNeeded <= s->limitPlusSlop - s->frontier);
	leave (s);
}

/* ---------------------------------------------------------------- */
/*                         GC_arrayAllocate                         */
/* ---------------------------------------------------------------- */

static inline W64 w64align (W64 w) {
 	return ((w + 3) & ~ 3);
}

pointer GC_arrayAllocate (GC_state s, W32 ensureBytesFree, W32 numElts, 
				W32 header) {
	uint numPointers;
	uint numNonPointers;
	uint tag;
	uint eltSize;
	W64 arraySize64;
	W32 arraySize;
	W32 *frontier;
	W32 *last;
	pointer res;

	SPLIT_HEADER();
	assert ((numPointers == 1 and numNonPointers == 0)
			or (numPointers == 0 and numNonPointers > 0));
	eltSize = numPointers * POINTER_SIZE + numNonPointers;
	arraySize64 = 
		w64align((W64)eltSize * (W64)numElts + GC_ARRAY_HEADER_SIZE);
	if (arraySize64 >= 0x100000000llu)
		die ("Out of memory: cannot allocate array with %s bytes.\n",
			ullongToCommaString (arraySize64));
	arraySize = (W32)arraySize64;
	if (DEBUG_ARRAY)
		fprintf (stderr, "array with %s elts of size %u and total size %s.  ensure %s bytes free.\n",
			uintToCommaString (numElts), 
			(uint)eltSize, 
			uintToCommaString (arraySize),
			uintToCommaString (ensureBytesFree));
	if (arraySize >= s->oldGenArraySize) {
		enter (s);
		doGC (s,  arraySize, ensureBytesFree, FALSE);
		leave (s);
		frontier = (W32*)(s->heap.oldGen + s->heap.oldGenSize);
		last = (W32*)((pointer)frontier + arraySize);
		s->heap.oldGenSize += arraySize;
	} else {
		W32 require;

		require = arraySize + ensureBytesFree;
		if (require > s->limitPlusSlop - s->frontier) {
			enter (s);
			doGC (s, 0, require, FALSE);
			leave (s);
		}
		frontier = (W32*)s->frontier;
		last = (W32*)((pointer)frontier + arraySize);
		s->frontier = (pointer)last;
	}
	*frontier++ = 0; /* counter word */
	*frontier++ = numElts;
	*frontier++ = header;
	res = (pointer)frontier;
	if (1 == numPointers)
		for ( ; frontier < last; frontier++)
			*frontier = BOGUS_POINTER;
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

static inline uint threadBytes () {
	return wordAlign(HEADER_SIZE + sizeof(struct GC_thread));
}

static inline uint initialThreadBytes (GC_state s) {
	return threadBytes () + stackBytes (initialStackSize (s));
}

static inline GC_thread newThreadOfSize (GC_state s, uint stackSize) {
	GC_stack stack;
	GC_thread t;

	ensureFree (s, stackBytes (stackSize) + threadBytes ());
	stack = newStack (s, stackSize, FALSE);
	t = (GC_thread) object (s, THREAD_HEADER, threadBytes (), FALSE);
	t->bytesNeeded = 0;
	t->exnStack = BOGUS_EXN_STACK;
	t->stack = stack;
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%x = newThreadOfSize (%u)\n",
				(uint)t, stackSize);;
	return t;
}

static inline GC_thread copyThread (GC_state s, GC_thread from, uint size) {
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
	stackCopy (from->stack, to->stack);
	to->exnStack = from->exnStack;
	return to;
}

void GC_copyCurrentThread (GC_state s) {
	GC_thread t;
	GC_thread res;
	
	if (DEBUG_THREADS)
		fprintf (stderr, "GC_copyCurrentThread\n");
	enter (s);
	t = s->currentThread;
	res = copyThread (s, t, t->stack->used);
	assert (res->stack->reserved == res->stack->used);
	leave (s);
	if (DEBUG_THREADS)
		fprintf (stderr, "0x%08x = GC_copyCurrentThread\n", (uint)res);
	s->savedThread = res;
}

pointer GC_copyThread (GC_state s, pointer thread) {
	GC_thread res;
	GC_thread t;

	t = (GC_thread)thread;
	if (DEBUG_THREADS)
		fprintf (stderr, "GC_copyThread (0x%08x)\n", (uint)t);
	enter (s);
	assert (t->stack->reserved == t->stack->used);
	res = copyThread (s, t, stackNeedsReserved (s, t->stack));
	assert (stackTopIsOk (s, res->stack));
	leave (s);
	return (pointer)res;
}

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

static inline void initSignalStack (GC_state s) {
#if (defined (__linux__) || defined (__FreeBSD__))
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

/*
 * Set RAM and SWAP size.
 * Note the total amount of RAM is multiplied by ramSlop so that we don't
 * use all of memory or start paging.
 *
 * Ensure that s->totalRam + s->totalSwap < 4G.
 */

#if (defined (__linux__))
#include <sys/sysinfo.h>
/* struct sysinfo copied from /usr/include/linux/kernel.h on a 2.4 kernel
 * because we need mem_unit.
 * On older kernels, it will be guaranteed to be zero, and we test for that
 * below.
 */
struct Msysinfo {
	long uptime;			/* Seconds since boot */
	unsigned long loads[3];		/* 1, 5, and 15 minute load averages */
	unsigned long totalram;		/* Total usable main memory size */
	unsigned long freeram;		/* Available memory size */
	unsigned long sharedram;	/* Amount of shared memory */
	unsigned long bufferram;	/* Memory used by buffers */
	unsigned long totalswap;	/* Total swap space size */
	unsigned long freeswap;		/* swap space still available */
	unsigned short procs;		/* Number of current processes */
	unsigned long totalhigh;	/* Total high memory size */
	unsigned long freehigh;		/* Available high memory size */
	unsigned int mem_unit;		/* Memory unit size in bytes */
	char _f[20-2*sizeof(long)-sizeof(int)];	/* Padding: libc5 uses this.. */
};
static inline void
setMemInfo (GC_state s)
{
	struct Msysinfo	sbuf;
	W32 maxMem;
	W64 tmp;
	uint memUnit;

	maxMem = 0x100000000llu - s->pageSize;
	unless (0 == sysinfo((struct sysinfo*)&sbuf))
		diee ("sysinfo failed");
	memUnit = sbuf.mem_unit;
	/* On 2.2 kernels, mem_unit is not defined, but will be zero, so go
	 * ahead and pretend it is one.
	 */
	if (0 == memUnit)
		memUnit = 1;
	tmp = memUnit * (W64)sbuf.totalram;
	s->totalRam = (tmp > (W64)maxMem) ? maxMem : (W32)tmp;
	maxMem = maxMem - s->totalRam;
	tmp = memUnit * (W64)sbuf.totalswap;
	s->totalSwap = (tmp > (W64)maxMem) ? maxMem : (W32)tmp;
}
#elif (defined (__CYGWIN__))
#include <windows.h>
static inline void
setMemInfo (GC_state s) {
	MEMORYSTATUS ms; 

	GlobalMemoryStatus(&ms); 
	s->totalRam = ms.dwTotalPhys;
	s->totalSwap = ms.dwTotalPageFile;
}
#elif (defined (__FreeBSD__))

/* returns total amount of swap available */
static int 
get_total_swap() 
{
        static char buffer[256];
        FILE *file;
        int total_size = 0;

        file = popen("/usr/sbin/swapinfo -k | awk '{ print $4; }'\n", "r");
        if (file == NULL) 
                diee ("swapinfo failed");

        /* skip header */
        fgets(buffer, 255, file);

        while (fgets(buffer, 255, file) != NULL) { 
                total_size += atoi(buffer);
        }

        pclose(file);

        return total_size * 1024;
}

/* returns total amount of memory available */
static int 
get_total_mem() 
{
        static char buffer[256];
        FILE *file;
        int total_size = 0;

        file = popen("/sbin/sysctl hw.physmem | awk '{ print $2; }'\n", "r");
        if (file == NULL) 
                diee ("sysctl failed");


        fgets(buffer, 255, file);

        pclose(file);

        return atoi(buffer);
}

static inline void setMemInfo (GC_state s) {
	s->totalRam = get_total_mem();
	s->totalSwap = get_total_swap();
}

#endif /* definition of setMemInfo */

static void usage (string s) {
	die ("Usage: %s [@MLton [fixed-heap n[{k|m}]] [gc-messages] [gc-summary] [load-world file] [ram-slop x] --] args", 
		s);
}

static float stringToFloat (string s) {
	float f;

	sscanf (s, "%f", &f);
	return f;
}

static uint stringToBytes (string s) {
	char c;
	uint result;
	int i, m;
	
	result = 0;
	i = 0;

	while ((c = s[i++]) != '\000') {
		switch (c) {
		case 'm':
			if (s[i] == '\000') 
				result = result * 1048576;
			else return 0;
			break;
		case 'k':
			if (s[i] == '\000') 
				result = result * 1024;
			else return 0;
			break;
		default:
			m = (int)(c - '0');
			if (0 <= m and m <= 9)
				result = result * 10 + m;
			else return 0;
		}
	}
	
	return result;
}

static inline void setInitialBytesLive (GC_state s) {
	int i;
	int numElements;

	s->bytesLive = 0;
	for (i = 0; s->intInfInits[i].mlstr != NULL; ++i) {
		numElements = strlen (s->intInfInits[i].mlstr);
		s->bytesLive +=
			GC_ARRAY_HEADER_SIZE + WORD_SIZE // for the sign
			+ ((0 == numElements) 
				? POINTER_SIZE 
				: wordAlign (numElements));
	}
	for (i = 0; s->stringInits[i].str != NULL; ++i) {
		numElements = s->stringInits[i].size;
		s->bytesLive +=
			GC_ARRAY_HEADER_SIZE
			+ ((0 == numElements) 
				? POINTER_SIZE 
				: wordAlign (numElements));
	}
}

#include "IntInf.h"
#include "gmp.h"
/*
 * For each entry { globalIndex, mlstr } in the inits array (which is terminated
 * by one with an mlstr of NULL), set
 *	state->globals[globalIndex]
 * to the corresponding IntInf.int value.
 * On exit, the GC_state pointed to by state is adjusted to account for any
 * space used.
 */
static inline void initIntInfs (GC_state s) {
	struct GC_intInfInit *inits;
	pointer frontier;
	char	*str;
	uint	slen,
		llen,
		alen,
		i;
	bool	neg,
		hex;
	bignum	*bp;
	char	*cp;

	inits = s->intInfInits;
	frontier = s->frontier;
	for (; (str = inits->mlstr) != NULL; ++inits) {
		assert (inits->globalIndex < s->numGlobals);
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
		assert(slen > 0);
		bp = (bignum *)frontier;
		cp = (char *)&bp->limbs[llen];
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
		assert(alen <= llen);
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
		frontier = (pointer)&bp->limbs[alen];
	}
	s->frontier = frontier;
}

static inline void initStrings (GC_state s) {
	struct GC_stringInit *inits;
	pointer frontier;
	int i;

	inits = s->stringInits;
	frontier = s->frontier;
	for (i = 0; inits[i].str != NULL; ++i) {
		uint numElements, numBytes;

		numElements = inits[i].size;
		numBytes = GC_ARRAY_HEADER_SIZE
			+ ((0 == numElements) 
				? POINTER_SIZE 
				: wordAlign(numElements));
		assert (numBytes <= s->heap.start + s->heap.size - frontier);
		*(word*)frontier = 0; /* counter word */
		*(word*)(frontier + WORD_SIZE) = numElements;
		*(word*)(frontier + 2 * WORD_SIZE) = STRING_HEADER;
		s->globals[inits[i].globalIndex] = 
			frontier + GC_ARRAY_HEADER_SIZE;
		if (DEBUG_DETAILED)
			fprintf (stderr, "allocated string at 0x%x\n",
					(uint)s->globals[inits[i].globalIndex]);
		{
			int j;

			for (j = 0; j < numElements; ++j)
				*(frontier + GC_ARRAY_HEADER_SIZE + j) 
					= inits[i].str[j];
		}
		frontier += numBytes;
	}
	if (DEBUG_DETAILED)
		fprintf (stderr, "frontier after string allocation is 0x%08x\n",
				(uint)frontier);
	s->frontier = frontier;
}

static void newWorld (GC_state s)
{
	int i;

	assert (isWordAligned (sizeof (struct GC_thread)));
	for (i = 0; i < s->numGlobals; ++i)
		s->globals[i] = (pointer)BOGUS_POINTER;
	setInitialBytesLive (s);
	heapCreate (s, &s->heap, heapDesiredSize (s, s->bytesLive, 0),
			s->bytesLive);
	s->frontier = s->heap.oldGen;
	initIntInfs (s);
	initStrings (s);
	assert (s->frontier - s->heap.oldGen <= s->bytesLive);
	s->heap.oldGenSize = s->frontier - s->heap.oldGen;
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
	char c;
	
	file = sfopen (fileName, "rb");
	until ((c = fgetc (file)) == worldTerminator or EOF == c);
	if (EOF == c) die ("Invalid world.");
	magic = sfreadUint (file);
	unless (s->magic == magic)
		die ("Invalid world: wrong magic number.");
	oldGen = (pointer) sfreadUint (file);
	s->heap.oldGenSize = sfreadUint (file);
	s->currentThread = (GC_thread) sfreadUint (file);
	s->signalHandler = (GC_thread) sfreadUint (file);
       	heapCreate (s, &s->heap, heapDesiredSize (s, s->heap.oldGenSize, 0),
			s->heap.oldGenSize);
	setNursery (s, 0, 0);
	sfread (s->heap.oldGen, 1, s->heap.oldGenSize, file);
	(*s->loadGlobals) (file);
	unless (EOF == fgetc (file))
		die ("Invalid world: junk at end of file.");
	fclose (file);
	/* translateHeap must occur after loading the heap and globals, since it
	 * changes pointers in all of them.
	 */
	translateHeap (s, oldGen, s->heap.oldGen, s->heap.oldGenSize);
	setStack (s);
}

int GC_init (GC_state s, int argc, char **argv) {
	char *worldFile;
	int i;

	s->bytesAllocated = 0;
	s->bytesCopied = 0;
	s->bytesCopiedMinor = 0;
	s->bytesMarkCompacted = 0;
	s->canHandle = 0;
	s->cardSize = 0x1 << s->cardSizeLog2;
	s->copyRatio = 2.0;
	s->currentThread = BOGUS_THREAD;
	s->generationalRatio = 4.0;
	s->growRatio = 1.5;
	s->inSignalHandler = FALSE;
	s->isOriginal = TRUE;
	s->liveRatio = 8.0;
	s->markCompactRatio = 1.5;
	s->markedCards = 0;
	s->maxBytesLive = 0;
	s->maxHeap = 0;
	s->maxHeapSizeSeen = 0;
	s->maxPause = 0;
	s->maxStackSizeSeen = 0;
	s->messages = FALSE;
	s->minorBytesScanned = 0;
	s->minorBytesSkipped = 0;
	s->numCopyingGCs = 0;
	s->numLCs = 0;
	s->numMarkCompactGCs = 0;
	s->numMinorGCs = 0;
	s->numMinorsSinceLastMajor = 0;
	s->nurseryRatio = 10.0;
	s->oldGenArraySize = 0x100000;
	s->pageSize = getpagesize ();
	s->ramSlop = 0.80;
	s->savedThread = BOGUS_THREAD;
	s->signalHandler = BOGUS_THREAD;
	s->signalIsPending = FALSE;
	s->startTime = currentTime ();
	s->summary = FALSE;
	heapInit (&s->heap);
	heapInit (&s->heap2);
	sigemptyset (&s->signalsHandled);
	initSignalStack (s);
	sigemptyset (&s->signalsPending);
	rusageZero (&s->ru_gc);
	rusageZero (&s->ru_gcCopy);
	rusageZero (&s->ru_gcMarkCompact);
	rusageZero (&s->ru_gcMinor);
 	readProcessor ();
	worldFile = NULL;
	i = 1;
	if (argc > 1 and (0 == strcmp (argv [1], "@MLton"))) {
		bool done;

		/* process @MLton args */
		i = 2;
		done = FALSE;
		while (!done) {
			if (i == argc)
				usage(argv[0]);
			else {
				string arg;

				arg = argv[i];
				if (0 == strcmp (arg, "copy-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->copyRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp(arg, "fixed-heap")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->useFixedHeap = TRUE;
					s->fixedHeapSize =
						stringToBytes (argv[i++]);
				} else if (0 == strcmp (arg, "gc-messages")) {
					++i;
					s->messages = TRUE;
				} else if (0 == strcmp (arg, "gc-summary")) {
					++i;
					s->summary = TRUE;
				} else if (0 == strcmp (arg, "generational-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->generationalRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "grow-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->growRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "live-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->liveRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "load-world")) {
					++i;
					s->isOriginal = FALSE;
					if (i == argc) 
						usage (argv[0]);
					worldFile = argv[i++];
				} else if (0 == strcmp (arg, "max-heap")) {
					++i;
					if (i == argc) 
						usage (argv[0]);
					s->useFixedHeap = FALSE;
					s->maxHeap = stringToBytes (argv[i++]);
				} else if (0 == strcmp (arg, "mark-compact-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->markCompactRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "nursery-ratio")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->nurseryRatio =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "ram-slop")) {
					++i;
					if (i == argc)
						usage (argv[0]);
					s->ramSlop =
						stringToFloat (argv[i++]);
				} else if (0 == strcmp (arg, "--")) {
					++i;
					done = TRUE;
				} else if (i > 1)
					usage (argv[0]);
			        else done = TRUE;
			}
		}
	}
	unless (ratiosOk (s))
		die ("invalid ratios");
	setMemInfo (s);
	s->ram = s->ramSlop * s->totalRam;
	if (DEBUG or DEBUG_RESIZING)
		fprintf (stderr, "totalRam = %u  totalSwap = %u\n",
				s->totalRam, s->totalSwap);
	if (s->isOriginal)
		newWorld (s);
	else
		loadWorld (s, worldFile);
	assert (mutatorInvariant (s));
	return i;
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
			uintToCommaString (1000.0 * (float)bytes/(float)ms));
	fprintf (out, "\n");
}

inline void GC_done (GC_state s) {
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
#if METER
		{
			int i;
			for (i = 0; i < cardof(sizes); ++i) {
				if (0 != sizes[i])
					fprintf (out, "COUNT[%d]=%d\n", i, sizes[i]);
		  	}
		}
#endif
	}
	heapRelease (s, &s->heap);
	heapRelease (s, &s->heap2);
}

void GC_finishHandler (GC_state s) {
	assert (s->canHandle == 1);
	s->inSignalHandler = FALSE;	
	sigemptyset (&s->signalsPending);
	unblockSignals (s);
}

/* GC_handler sets s->limit = 0 so that the next limit check will fail. 
 * Signals need to be blocked during the handler (i.e. it should run atomically)
 * because sigaddset does both a read and a write of s->signalsPending.
 * The signals are blocked by Posix_Signal_handle (see Posix/Signal/Signal.c).
 */
inline void GC_handler (GC_state s, int signum) {
	if (DEBUG_SIGNALS)
		fprintf (stderr, "GC_handler  signum = %d\n", signum);
	if (0 == s->canHandle) {
		if (DEBUG_SIGNALS)
			fprintf (stderr, "setting limit = 0\n");
		s->limit = 0;
	}
	sigaddset (&s->signalsPending, signum);
	s->signalIsPending = TRUE;
	if (DEBUG_SIGNALS)
		fprintf (stderr, "GC_handler done\n");
}

uint GC_size (GC_state s, pointer root) {
	uint res;

	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "GC_size marking\n");
	res = mark (s, root, MARK_MODE);
	if (DEBUG_MARK_COMPACT)
		fprintf (stderr, "GC_size unmarking\n");
	mark (s, root, UNMARK_MODE);
	return res;
}

void GC_saveWorld (GC_state s, int fd) {
	char buf[80];

	if (DEBUG)
		fprintf (stderr, "Save world.\n");
	enter (s);
	/* Compact the heap. */
	doGC (s, 0, 0, TRUE);
	sprintf (buf,
		"Heap file created by MLton.\noldGen = 0x%08x\nbytesLive = %u\n",
		(uint)s->heap.oldGen, (uint)s->bytesLive);
	swrite (fd, buf, 1 + strlen(buf)); /* +1 to get the '\000' */
	swriteUint (fd, s->magic);
	swriteUint (fd, (uint)s->heap.oldGen);
	swriteUint (fd, (uint)s->heap.oldGenSize);
	swriteUint (fd, (uint)s->currentThread);
	swriteUint (fd, (uint)s->signalHandler);
 	swrite (fd, s->heap.oldGen, s->heap.oldGenSize);
	(*s->saveGlobals) (fd);
	leave (s);
}

void GC_pack (GC_state s) {
	enter (s);
	if (DEBUG or s->messages)
		fprintf (stderr, "Packing heap of size %s.\n",
				uintToCommaString (s->heap.size));
	/* Could put some code here to skip the GC if there hasn't been much
	 * allocated since the last collection.  But you would still need to 
	 * do a minor GC to make all objects contiguous.
 	 */
	doGC (s, 0, 0, TRUE);
	heapShrink (s, &s->heap, s->bytesLive * 1.1);
	setNursery (s, 0, 0);
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
	minorGC (s);
	resizeHeap (s, s->heap.oldGenSize);
	if (DEBUG or s->messages)
		fprintf (stderr, "Unpacked heap of size %s.\n",
				uintToCommaString (s->heap.size));
	leave (s);
}
