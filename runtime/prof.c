/*
 * The only externally visible function in this file is
 *	void	startProf(void)
 * which starts up profiling and arranges (via atexit()) to write out the
 * results when the program exits.  It monitors the program counter 100
 * times per second and increments an entry in array of uints.  Thus
 * counters cannot possibly overflow for 2^32 / 100 seconds or a bit over
 * 1 CPU year.
 * On exit the file mlmon.out is created.  This file consists of:
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
 * Note, perhaps this code should use mmap()/munmap() instead of
 * malloc()/free() for the array of bins.
 * I do not believe that it should use stdio for the mlmon.out file though.
 * By the time endProf() gets called, the stdio system may be shut down.
 * Of course in that case the die() calls would probably fail.
 */
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <signal.h>
#include <ucontext.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "my-lib.h"

#ifndef EIP
#define EIP	14
#endif
#define	MAGIC	"MLton prof\n"

struct	pdata {
	char	magic[12];
	uint	start,
		limit,
		unknown;
};


extern void	_start(void),
		etext(void);


static uint	*buff = NULL,
		card = 0,
		unknown = 0;

static void	catcher(int sig, siginfo_t *sip, ucontext_t *ucp),
		endProf(void);

void
startProf(void)
{
	struct sigaction	sa;
	struct itimerval	tv;

	card = (uint)&etext - (uint)&_start;
	buff = (uint *)malloc(card * sizeof(*buff));
	if (buff == NULL)
		die("Out of memory");
	memset(buff, 0, card * sizeof(*buff));
	sa.sa_handler = (void (*)(int))catcher;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
	unless (sigaction(SIGPROF, &sa, NULL) == 0)
		diee("sigaction() failed");
	tv.it_interval.tv_sec = 0;
	tv.it_interval.tv_usec = 10000;
	tv.it_value = tv.it_interval;
	unless (setitimer(ITIMER_PROF, &tv, NULL) == 0)
		diee("setitimer() failed");
	atexit(endProf);
}


/*
 * Called on each SIGPROF interrupt.
 */
static void
catcher(int sig, siginfo_t *sip, ucontext_t *ucp)
{
	uint	pc;

	pc = ucp->uc_mcontext.gregs[EIP] - (uint)&_start;
	if (pc < card)
		++buff[pc];
	else
		++unknown;
}

void
resetProf(void)
{
	assert((card != 0) and (buff != NULL)); 
	memset(buff, 0, card * sizeof(*buff));
}

void
writeProf(char* filename)
{
	int			fd;
	struct pdata		pd;

	fd = creat(filename, 0666);
	if (fd < 0)
		diee("Cannot create mlmon.out");
	assert(sizeof(pd.magic) == sizeof(MAGIC));
	strcpy(pd.magic, MAGIC);
	pd.start = (uint)&_start;
	pd.limit = (uint)&etext;
	pd.unknown = unknown;
	unless ((write(fd, &pd, sizeof(pd)) == sizeof(pd))
	and (write(fd, buff, card * sizeof(*buff)) == card * sizeof(*buff)))
		diee("write() failed");
	unless (close(fd) == 0)
		diee("close() failed");
}

/*
 * Write out the accumulated profiling data.
 * Note, we might be unable to open the mlmon.out file at this point.
 * We could have tried to create it on startup, but then we would destroy
 * any old mlmon.out, and if the program dies we would be replacing it with
 * a zero-length file.  This could be worked around, but doesn't seem
 * worth it.
 */
static void
endProf(void)
{
	struct itimerval	tv;

	tv.it_value.tv_sec = 0;
	tv.it_value.tv_usec = 0;
	unless (setitimer(ITIMER_PROF, &tv, NULL) == 0)
		diee("setitimer() failed");
	writeProf("mlmon.out");
}
