#define _GNU_SOURCE

#include "platform.h"

#include "getText.c"
#include "mkdir2.c"
#include "showMem.linux.c"
#include "ssmmap.c"
#include "totalRam.sysconf.c"
#include "use-mmap.c"

#ifndef EIP
#define EIP	14
#endif

static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
	GC_handleSigProf ((pointer) ucp->uc_mcontext.gregs[EIP]);
}

void setSigProfHandler (struct sigaction *sa) {
	sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
	sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

/* Work around Linux kernel bugs associated with the user and system times. */

int fixedGetrusage (int who, struct rusage *rup) {
	struct tms	tbuff;
	int		res;
	clock_t		user,
			sys;
	static bool	first = TRUE;
	static long	hz;

	if (first) {
		first = FALSE;
		hz = sysconf (_SC_CLK_TCK);
	}
	res = getrusage (who, rup);
	unless (res == 0)
		return (res);
	if (times (&tbuff) == -1)
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

/* We need the value of MREMAP_MAYMOVE, which should come from sys/mman.h, but
 * isn't there.  It is in linux/mman.h, but we can't #include that here, because
 * kernel headers don't mix with system headers.  We could create a separate
 * file, include the kernel headers there, and define a global.  But there
 * sometimes seem to be problems including kernel headers, so the easiest thing
 * to do is just define MREMAP_MAYMOVE.
 */
#define MREMAP_MAYMOVE 1

void *remap (void *old,  size_t oldSize, size_t newSize) {
	return mremap (old, oldSize, newSize, MREMAP_MAYMOVE);
}
