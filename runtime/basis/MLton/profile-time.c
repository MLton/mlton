#if (defined (__linux__) || defined (__FreeBSD__))
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <ucontext.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

#ifndef EIP
#define EIP	14
#endif

extern struct GC_state gcState;

Pointer MLton_ProfileTime_current () {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "0x%08x = MLton_ProfileTime_current ()\n",
				(uint)s->profileTime);
	return (Pointer)s->profileTime;
}

void MLton_ProfileTime_setCurrent (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "MLton_ProfileTime_setCurrent (0x%08x)\n",
				(uint)d);
	s->profileTime = (typeof(s->profileTime))d;
}

Pointer MLton_ProfileTime_Data_malloc (void) {
	GC_state s;
	GC_profileTime pt;

	s = &gcState;
	pt = GC_profileTimeNew (s);
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "0x%08x = MLton_ProfileTimeData_malloc ()\n",
				(uint)pt);
	return (Pointer)pt;
}

void MLton_ProfileTime_Data_free (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "MLton_ProfileTime_Data_free (0x%08x)",
				(uint)d);
	GC_profileTimeFree (s, (GC_profileTime)d);
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "\n");
}

void MLton_ProfileTime_Data_write (Pointer d, Word fd) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_TIME) 
		fprintf (stderr, "MLton_ProfileTime_Data_Write (0x%08x, %ld)\n",
				(uint)d, fd);
	GC_profileTimeWrite (s, (GC_profileTime) d, fd);
}

static void incAndMark (GC_state s, uint sourceSeqsIndex) {
	uint i;
	uint length;
	uint source;
	uint *sourceSeq;

	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "incAndMark (%u)\n", sourceSeqsIndex);
	assert (sourceSeqsIndex < s->sourceSeqsSize);
	sourceSeq = s->sourceSeqs [sourceSeqsIndex];
	length = sourceSeq[0];
	for (i = 1; i <= length; ++i) {
		source = sourceSeq[i];
		if (DEBUG_PROFILE_TIME)
			fprintf (stderr, "reached %s ", s->sources[source]);
		if (s->sourceIsOnStack[source]) {
			if (DEBUG_PROFILE_TIME)
				fprintf (stderr, " already on stack\n");
		} else {
			if (DEBUG_PROFILE_TIME)
				fprintf (stderr, "bumping\n");
			s->sourceIsOnStack[source] = TRUE;
			s->profileTime->ticks[source]++;
		}
	}
}

static void incAndMarkFrame (GC_state s, uint frameSourcesIndex) {
	incAndMark (s, s->frameSources[frameSourcesIndex]);
}

static void unmark (GC_state s, uint sourceSeqsIndex) {
	uint i;
	uint length;
	uint source;
	uint *sourceSeq;

	sourceSeq = s->sourceSeqs [sourceSeqsIndex];
	length = sourceSeq[0];
	for (i = 1; i <= length; ++i) {
		source = sourceSeq[i];
		s->sourceIsOnStack [source] = FALSE;
	}
}

static void unmarkFrame (GC_state s, uint frameSourcesIndex) {
	unmark (s, s->frameSources[frameSourcesIndex]);
}

/*
 * Called on each SIGPROF interrupt.
 */
static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
	GC_state s;
	pointer pc;
	uint *sourceSeq;
	uint sourceSeqsIndex;

	s = &gcState;
	s->profileTime->totalTicks++;
	if (s->amInGC) {
		s->profileTime->ticks [SOURCES_INDEX_GC]++; 
		return;
	}
#if (defined (__linux__))
        pc = (pointer) ucp->uc_mcontext.gregs[EIP];
#elif (defined (__FreeBSD__))
	pc = (pointer) ucp->uc_mcontext.mc_eip;
#else
#error pc not defined
#endif
	if (DEBUG_PROFILE_TIME)
		fprintf (stderr, "pc = 0x%08x\n", (uint)pc);
	if (s->textStart <= pc and pc < s->textEnd) {
		sourceSeqsIndex = s->textSources [pc - s->textStart];
	} else {
		sourceSeqsIndex = SOURCE_SEQ_UNKNOWN;
	}
	assert (sourceSeqsIndex < s->sourceSeqsSize);
	switch (s->profileStyle) {
	case PROFILE_CUMULATIVE:
		/* Walk all the stack frames. */
		incAndMark (s, sourceSeqsIndex);
		GC_foreachStackFrame (s, incAndMarkFrame);
		unmark (s, sourceSeqsIndex);
		GC_foreachStackFrame (s, unmarkFrame);
	break;
	case PROFILE_CURRENT:
		sourceSeq = s->sourceSeqs [sourceSeqsIndex];
		assert (sourceSeq [0] > 0);
		assert (sourceSeq [1] < s->sourcesSize);
		s->profileTime->ticks [sourceSeq [1]]++;
	break;
	}
}

void MLton_ProfileTime_init (void) {
/*
 * Install catcher, which handles SIGPROF and updates the entry in current
 * corresponding to the program counter.
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
	struct sigaction	sa;

	sa.sa_handler = (void (*)(int))catcher;
	sigemptyset (&sa.sa_mask);
	sa.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
	unless (sigaction (SIGPROF, &sa, NULL) == 0)
		diee ("sigaction() failed");
}

#elif (defined (__CYGWIN__))

/* No profiling on Cygwin. 
 * There is a check in mlton/main/main.sml to make sure that profiling is never
 * turned on on Cygwin.
 */

#else

#error profiling not defined

#endif
