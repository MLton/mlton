#include <string.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	DEBUG_PROFILE = FALSE,
};

extern struct GC_state gcState;

void MLton_Profile_Data_write (Pointer p, Word fd) {
	GC_profileWrite (&gcState, (GC_profile)p, (int)fd);
}

void MLton_Profile_Data_free (Pointer p) {
	GC_profileFree (&gcState, (GC_profile)p);
}

Pointer MLton_Profile_Data_malloc (void) {
	return (Pointer)GC_profileNew (&gcState);
}

Pointer MLton_Profile_current (void) {
	GC_state s;
	Pointer res;

	s = &gcState;
	res = (Pointer)s->profile;
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%0x8 = MLton_Profile_current ()\n", 
				(uint)res);
	return res;
}

void MLton_Profile_done () {
	int i;
	GC_state s;
	GC_profile p;

	if (DEBUG_PROFILE) 
		fprintf (stderr, "MLton_Profile_done ()\n");
	s = &gcState;
	GC_profileDone (s);
	p = s->profile;
	if (s->profileStack) {
		for (i = 0; i < s->sourcesSize; ++i) {
			if (p->stackCount[i] > 0) {
				if (DEBUG_PROFILE)
					fprintf (stderr, "done leaving %s\n", 
							s->sources[i]);
				p->count[i] += p->total - p->lastTotal[i];
			}
		}
	}
}

void MLton_Profile_enter (Word sourceSeqsIndex) {
	int i;
	GC_profile p;
	uint sourceIndex;
	uint *sourceSeq;

	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_enter (%u)\n",
				(uint)sourceSeqsIndex);
	assert (gcState.profileStack);
	assert (sourceSeqsIndex < gcState.sourceSeqsSize);
	p = gcState.profile;
	sourceSeq = gcState.sourceSeqs[sourceSeqsIndex];
	for (i = 1; i <= sourceSeq[0]; ++i) {
		sourceIndex = sourceSeq[i];
		if (DEBUG_PROFILE)
			fprintf (stderr, "entering %s\n", 
					gcState.sources[sourceIndex]);
		if (0 == p->stackCount[sourceIndex])
			p->lastTotal[sourceIndex] = p->total;
		p->stackCount[sourceIndex]++;
	}
}

void MLton_Profile_inc (Word amount) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_inc (%u) currentSource = %u\n",
				(uint)amount,
				s->currentSource);
	if (s->amInGC) {
		if (DEBUG_PROFILE) 
			fprintf (stderr, "amInGC\n");
		s->profile->count [SOURCES_INDEX_GC] += amount;
	} else if (s->profileStack) {
		MLton_Profile_enter (s->currentSource);
		s->profile->total += amount;
		MLton_Profile_leave (s->currentSource);
	} else {
		uint source;
		uint *sourceSeq;

		s->profile->total += amount;
		sourceSeq = s->sourceSeqs[s->currentSource];
		source = sourceSeq[sourceSeq[0]];
		if (DEBUG_PROFILE)
			fprintf (stderr, "bumping %s by %u\n",
					s->sources[source], 
					(uint)amount);
		s->profile->count[source] += amount;
	}
}

void MLton_Profile_leave (Word sourceSeqsIndex) {
	int i;
	GC_profile p;
	uint sourceIndex;
	uint *sourceSeq;

	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_leave (%u)\n",
				(uint)sourceSeqsIndex);
	assert (gcState.profileStack);
	assert (sourceSeqsIndex < gcState.sourceSeqsSize);
	p = gcState.profile;
	sourceSeq = gcState.sourceSeqs[sourceSeqsIndex];
	for (i = sourceSeq[0]; i > 0; --i) {
		sourceIndex = sourceSeq[i];
		if (DEBUG_PROFILE)
			fprintf (stderr, "leaving %s\n",
					gcState.sources[sourceIndex]);
		assert (p->stackCount[sourceIndex] > 0);
		p->stackCount[sourceIndex]--;
		if (0 == p->stackCount[sourceIndex])
			p->count[sourceIndex] += 
				p->total - p->lastTotal[sourceIndex];
	}
}

void MLton_Profile_setCurrent (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_setCurrent (0x%08x)\n", (uint)d);
	s->profile = (GC_profile)d;
}
