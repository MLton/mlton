#if (defined (__linux__) || defined (__CYGWIN__) || defined (__FreeBSD__))
#include <string.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	PROFILE_ALLOC_GC = 0,
};

extern struct GC_state gcState;

Pointer MLton_ProfileAlloc_current (void) {
	GC_state s;
	Pointer res;

	s = &gcState;
	res = (Pointer)s->profileAlloc;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "0x%0x8 = MLton_ProfileAlloc_current ()\n",
				(uint)res);
	return res;
}

void MLton_ProfileAlloc_setCurrent (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_setCurrent (0x%08x)\n",
				(uint)d);
	s->profileAlloc = (GC_profileAlloc)d;
}

void MLton_ProfileAlloc_done () {
	int i;
	GC_state s;
	GC_profileAlloc pa;

	if (DEBUG_PROFILE_ALLOC) 
		fprintf (stderr, "MLton_ProfileAlloc_done ()\n");
	s = &gcState;
	pa = s->profileAlloc;
	assert (s->profileAllocIsOn);
	s->profileAllocIsOn = FALSE;
	switch (s->profileStyle) {
	case PROFILE_CUMULATIVE:
		for (i = 0; i < s->sourcesSize; ++i) {
			if (pa->stackCount[i] > 0) {
				if (DEBUG_PROFILE_ALLOC)
					fprintf (stderr, "leaving %s\n", s->sources[i]);
				pa->bytesAllocated[i] +=
					pa->totalBytesAllocated - pa->lastTotal[i];
			}
		}
	break;
	case PROFILE_CURRENT:
	break;
	}
}

void MLton_ProfileAlloc_inc (Word amount) {
	GC_state s;

	assert (s->profileAllocIsOn);
	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_inc (%u, %u)\n",
				s->currentSource,
				(uint)amount);
	if (s->amInGC) {
		if (DEBUG_PROFILE_ALLOC) 
			fprintf (stderr, "amInGC\n");
		s->profileAlloc->bytesAllocated [SOURCES_INDEX_GC] += amount;
	} else {
		s->profileAlloc->totalBytesAllocated += amount;
		switch (s->profileStyle) {
		case PROFILE_CUMULATIVE:
		break;
		case PROFILE_CURRENT:
			s->profileAlloc->bytesAllocated [s->currentSource] 
				+= amount;
		break;
		}
	}
}

void MLton_ProfileAlloc_incLeaveEnter (Word amount, Word leave, Word enter) {
	int i;
	GC_profileAlloc pa;
	GC_state s;
	uint sourceIndex;
	uint *sourceSeq;

	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_incLeaveEnter (%u, %u, %u)\n",
				(uint)amount, (uint)leave, (uint)enter);
	unless (s->profileAllocIsOn)
		return;
	MLton_ProfileAlloc_inc (amount);
	switch (s->profileStyle) {
	case PROFILE_CUMULATIVE:
		pa = s->profileAlloc;
		/* Leave. */
		sourceSeq = s->sourceSeqs[leave];
		for (i = 1; i <= sourceSeq[0]; ++i) {
			sourceIndex = sourceSeq[i];
			assert (pa->stackCount[sourceIndex] > 0);
			pa->stackCount[sourceIndex]--;
			if (DEBUG_PROFILE_ALLOC)
				fprintf (stderr, "leaving %s",
						s->sources[sourceIndex]);
			if (0 == pa->stackCount[sourceIndex]) {
				ullong alloc;

				alloc = pa->totalBytesAllocated 
					- pa->lastTotal[sourceIndex];
				if (DEBUG_PROFILE_ALLOC)
					fprintf (stderr, " with %llu bytes\n",
							alloc);
				pa->bytesAllocated[sourceIndex] += alloc;
			} else {
				if (DEBUG_PROFILE_ALLOC)
					fprintf (stderr, "\n");
			}
		}
		/* Enter. */
		sourceSeq = s->sourceSeqs[enter];
		for (i = 1; i < sourceSeq[0]; ++i) {
			sourceIndex = sourceSeq[i];
			if (DEBUG_PROFILE_ALLOC)
				fprintf (stderr, "entering %s\n",
						s->sources[sourceIndex]);
			if (0 == pa->stackCount[sourceIndex]) {
				pa->lastTotal[sourceIndex] =
					pa->totalBytesAllocated;
			}
			pa->stackCount[sourceIndex]++;
		}
	break;
	case PROFILE_CURRENT:
		sourceSeq = s->sourceSeqs[enter];
		/* The current source is the last function entered.  There is
		 * a hack in profile.fun to put the right thing there even if
		 * no functions are entered.
		 */
		s->currentSource = sourceSeq[sourceSeq[0]];
	break;
	}
}

void MLton_ProfileAlloc_setCurrentSource (Word sourceIndex) {
	gcState.currentSource = sourceIndex;
}

Pointer MLton_ProfileAlloc_Data_malloc (void) {
	Pointer res;
	GC_state s;

	s = &gcState;
	res = (Pointer)GC_profileAllocNew (s);
	return res;
}

void MLton_ProfileAlloc_Data_free (Pointer pa) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_Data_free (0x%08x)",
				(uint)pa);
	GC_profileAllocFree (s, (GC_profileAlloc)pa);
}

void MLton_ProfileAlloc_Data_write (Pointer pa, Word fd) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_Data_write (0x%08x, %u)\n",
				(uint)pa, (uint)fd);
	GC_profileAllocWrite (s, (GC_profileAlloc)pa, fd);
}

#else

#error profiling not defined

#endif
