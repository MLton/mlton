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

enum {
	DEBUG_PROFILE = FALSE,
};

extern struct GC_state gcState;

/* Current is an array of uints, one for each source position.
 * Counters cannot overflow for 2^32 / 100 seconds or a bit over 1 CPU year.
 */
static uint *current = NULL;

Pointer MLton_ProfileTime_current () {
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%08x = MLton_ProfileTime_current ()\n",
				(uint)current);
	return (Pointer)current;
}

void MLton_ProfileTime_setCurrent (Pointer d) {
	uint *data;

	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_ProfileTime_setCurrent (0x%08x)\n",
				(uint)d);
	data = (uint*)d;
	assert (data != NULL);
	current = data;
}

Pointer MLton_ProfileTime_Data_malloc (void) {
	/* Note, perhaps this code should use mmap()/munmap() instead of
	 * malloc()/free() for the array of bins.
	 */
	uint *data;
	
	data = (uint *)malloc (gcState.profileSourcesSize * sizeof(*data));
	if (data == NULL)
		die ("Out of memory");
	MLton_ProfileTime_Data_reset ((Pointer)data);
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%08x = MLton_ProfileTimeData_malloc ()\n",
				(uint)data);
	return (Pointer)data;
}

void MLton_ProfileTime_Data_free (Pointer d) {
	uint *data;

	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_ProfileTime_Data_free (0x%08x)",
				(uint)d);
	data = (uint*)d;
	assert (data != NULL);
	free (data);
	if (DEBUG_PROFILE)
		fprintf (stderr, "\n");
}

void MLton_ProfileTime_Data_reset (Pointer d) {
	uint *data;

	data = (uint*)d;
	assert (data != NULL); 
	memset (data, 0, gcState.profileSourcesSize * sizeof(*data));
}

static void writeString (int fd, string s) {
	swrite (fd, s, strlen(s));
	swrite (fd, "\n", 1);
}

static void writeWord (int fd, word w) {
	char buf[20];

	sprintf (buf, "0x%08x", w);
	writeString (fd, buf);
}

static void writeUint (int fd, uint w) {
	char buf[20];

	sprintf (buf, "%u", w);
	writeString (fd, buf);
}

void MLton_ProfileTime_Data_write (Pointer d, Word fd) {
/* Write a profile data array out to a file descriptor.
 *
 * The `unknown ticks' is a count of the number of times that the monitored
 * program counter was not in the range of a bin.  This almost certainly
 * corresponds to times when it was pointing at shared library code.
 * All values except for the initial string are unsigned integers in
 * the native machine format (4 bytes, little-endian).
 */
	uint *data;
	uint i;

	if (DEBUG_PROFILE) 
		fprintf (stderr, "MLton_ProfileTime_Data_Write (0x%08x, %ld)\n",
				(uint)d, fd);
	data = (uint*)d;
	writeString (fd, "MLton prof");
	writeString (fd, "time");
	writeWord (fd, gcState.magic);
	for (i = 0; i < gcState.profileSourcesSize; ++i)
		writeUint (fd, data[i]);
}

/*
 * Called on each SIGPROF interrupt.
 */
static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
	uint i;
	pointer pc;

#if (defined (__linux__))
        pc = (pointer) ucp->uc_mcontext.gregs[EIP];
#elif (defined (__FreeBSD__))
	pc = (pointer) ucp->uc_mcontext.mc_eip;
#else
#error pc not defined
#endif
	if (gcState.textStart <= pc and pc < gcState.textEnd)
		i = gcState.textSources [pc - gcState.textStart];
	else
		i = SOURCE_SEQ_UNKNOWN;
	assert (i < gcState.profileSourceSeqsSize);

	++current[gcState.profileSourceSeqs[i][1]];
	unless (TRUE or gcState.amInGC)
		free (GC_stackFrameIndices (&gcState));
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
