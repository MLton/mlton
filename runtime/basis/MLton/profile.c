#include <signal.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <signal.h>
#include <ucontext.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "mlton-basis.h"
#include "my-lib.h"

#ifndef EIP
#define EIP	14
#endif
#define	MAGIC	"MLton prof\n"

struct	pdata {
	char	magic[12];
	uint	start,
		limit;
};

extern void	_start(void),
		etext(void);

/* Current is an array of uints, where each element corresponds to a range of
 * addresses of the program counter.  Counters cannot possibly overflow for
 * 2^32 / 100 seconds or a bit over 1 CPU year.
 */
static uint	*current = NULL,
		card = 0;

void MLton_Profile_init (void)
{
	card = (uint)&etext - (uint)&_start + 1;
}

void MLton_Profile_setCurrent (Pointer d)
{
	uint *data = (uint*)d;

	assert(data != NULL);
	current = data;
}

Pointer MLton_Profile_Data_malloc (void)
/* Note, perhaps this code should use mmap()/munmap() instead of
 * malloc()/free() for the array of bins.
 */
{
	uint *data;

	assert(card != 0);
	data = (uint *)malloc(card * sizeof(*data));
	if (data == NULL)
		die("Out of memory");
	MLton_Profile_Data_reset((Pointer)data);
	return((Pointer)data);
}

void MLton_Profile_Data_free (Pointer d)
{
	uint *data = (uint*)d;

	assert((card != 0) and (data != NULL));
	free(data);
}

void MLton_Profile_Data_reset (Pointer d)
{
	uint *data;

	data = (uint*)d;

	assert((card != 0) and (data != NULL)); 
	memset(data, 0, card * sizeof(*data));
}

void MLton_Profile_Data_write (Pointer d, Word fd)
/* Write a profile data array out to a file descriptor
 * The file consists of:
 *	a 12 byte magic value ("MLton prof\n\000")
 *	the lowest address corresponding to a bin
 *	just past the highest address corresponding to a bin
 *	unknown ticks
 *	the bins
 * The `unknown ticks' is a count of the number of times that the monitored
 * program counter was not in the range of a bin.  This almost certainly
 * corresponds to times when it was pointing at shared library code.
 * All values except for the initial string are unsigned integers in
 * the native machine format (4 bytes, little-endian).
 */
{
	struct pdata		pd;
	uint *data = (uint*)d;

	assert(sizeof(pd.magic) == sizeof(MAGIC));
	strcpy(pd.magic, MAGIC);
	pd.start = (uint)&_start;
	pd.limit = (uint)&etext;
	unless ((write(fd, &pd, sizeof(pd)) == sizeof(pd))
	and (write(fd, data, card * sizeof(*data)) == card * sizeof(*data)))
		diee("write() failed");
}

/*
 * Called on each SIGPROF interrupt.
 */
static void
catcher(int sig, siginfo_t *sip, ucontext_t *ucp)
{
	uint	pc;

	pc = ucp->uc_mcontext.gregs[EIP];
	if (((uint)&_start <= pc) and (pc <= (uint)&etext))
		++current[pc - (uint)&_start + 1];
	else
		++current[0];
}

void MLton_Profile_installHandler (void)
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
{
	struct sigaction	sa;

	sa.sa_handler = (void (*)(int))catcher;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
	unless (sigaction(SIGPROF, &sa, NULL) == 0)
		diee("sigaction() failed");
}
