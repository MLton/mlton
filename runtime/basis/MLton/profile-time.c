#if (defined (__linux__) || defined (__FreeBSD__))
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <signal.h>
#include <ucontext.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

#ifndef EIP
#define EIP	14
#endif
#define	MAGIC	"MLton prof\n"

enum {
	DEBUG_PROFILE = FALSE,
};

extern struct GC_state gcState;

extern void	_start(void),
		etext(void);

/* Current is an array of uints, where each element corresponds to a range of
 * addresses of the program counter.  Counters cannot possibly overflow for
 * 2^32 / 100 seconds or a bit over 1 CPU year.
 */
static uint	*current = NULL,
		card = 0;

#define START ((uint)&_start)
#define END (uint)&etext

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
	
	assert(card != 0);
	data = (uint *)malloc (card * sizeof(*data));
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
	assert ((card != 0) and (data != NULL));
	free (data);
	if (DEBUG_PROFILE)
		fprintf (stderr, "\n");
}

void MLton_ProfileTime_Data_reset (Pointer d) {
	uint *data;

	data = (uint*)d;
	assert ((card != 0) and (data != NULL)); 
	memset (data, 0, card * sizeof(*data));
}

void MLton_ProfileTime_Data_write (Pointer d, Word fd) {
/* Write a profile data array out to a file descriptor
 * The file consists of:
 *	a 12 byte magic value ("MLton prof\n\000")
 *	the lowest address corresponding to a bin
 *	just past the highest address corresponding to a bin
 *	unknown ticks
 *	the nonzero bins
 *  		each bin is a 4 byte address followed by a 4 byte count
 * The `unknown ticks' is a count of the number of times that the monitored
 * program counter was not in the range of a bin.  This almost certainly
 * corresponds to times when it was pointing at shared library code.
 * All values except for the initial string are unsigned integers in
 * the native machine format (4 bytes, little-endian).
 */
	uint *data;
	uint i;

	if (DEBUG_PROFILE) 
		fprintf (stderr, "MLton_ProfileTime_Data_Write (0x%08x, %d)\n",
				(uint)d, fd);
	data = (uint*)d;
	swrite (fd, MAGIC, sizeof(MAGIC));
	swriteUint (fd, gcState.magic);
	swriteUint (fd, START);
	swriteUint (fd, END);
	swriteUint (fd, sizeof(*data));
	swriteUint (fd, MLPROF_KIND_TIME);
	unless (0 == data[card]) {
		swriteUint (fd, 0);
		swriteUint (fd, data[card]);
	}
	for (i = 0; i < card - 1; ++i) {
		unless (0 == data[i]) {
			swriteUint (fd, START + i);
			swriteUint (fd, data[i]); 
		}
	}
}

/*
 * Called on each SIGPROF interrupt.
 */
static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
	uint	pc;

#if (defined (__linux__))
        pc = ucp->uc_mcontext.gregs[EIP];
#elif (defined (__FreeBSD__))
	pc = ucp->uc_mcontext.mc_eip;
#else
#error pc not defined
#endif
	if (START <= pc and pc < END)
		++current[pc - START];
	else
		++current[card];
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


	card = END - START + 1; /* +1 for bin for unknown ticks*/
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

#endif /* (defined (__linux__)) */
