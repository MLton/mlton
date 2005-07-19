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

/* potentially correct for other archs:
 *  alpha: ucp->m_context.sc_pc
 *  arm: ucp->m_context.ctx.arm_pc
 *  ia64: ucp->m_context.sc_ip & ~0x3UL
 *  mips: ucp->m_context.sc_pc
 *  s390: ucp->m_context.sregs->regs.psw.addr
 */
static void catcher (int sig, siginfo_t* sip, void* mystery) {
#if (defined (__x86_64__))
#define REG_INDEX(NAME) (offsetof(struct sigcontext, NAME) / sizeof(greg_t))
#ifndef REG_RIP
#define REG_RIP REG_INDEX(rip) /* seems to be 16 */
#endif
	ucontext_t* ucp = (ucontext_t*)mystery;
	GC_handleSigProf ((pointer) ucp->uc_mcontext.gregs[REG_RIP]);
#elif (defined (__hppa__))
	ucontext_t* ucp = (ucontext_t*)mystery;
	GC_handleSigProf ((pointer) (ucp->uc_mcontext.sc_iaoq[0] & ~0x3UL));
#elif (defined (__ppc__)) || (defined (__powerpc__))
	ucontext_t* ucp = (ucontext_t*)mystery;
	GC_handleSigProf ((pointer) ucp->uc_mcontext.regs->nip);
#elif (defined (__sparc__))
	struct sigcontext* scp = (struct sigcontext*)mystery;
#if __WORDSIZE == 64
	GC_handleSigProf ((pointer) scp->sigc_regs.tpc);
#else
	GC_handleSigProf ((pointer) scp->si_regs.pc);
#endif
#elif (defined (__i386__))
	ucontext_t* ucp = (ucontext_t*)mystery;
	GC_handleSigProf ((pointer) ucp->uc_mcontext.gregs[EIP]);
#else
#error Profiling handler is missing for this architecture
#endif
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

/* ------------------------------------------------- */
/*                       Posix                       */
/* ------------------------------------------------- */

void Posix_IO_setbin (Fd fd) {
	die("Posix_IO_setbin not implemented");
}

void Posix_IO_settext (Fd fd) {
	die("Posix_IO_settext not implemented");
}

/* ------------------------------------------------- */
/*                      Process                      */
/* ------------------------------------------------- */

Pid MLton_Process_cwait (Pid pid, Pointer status) {
	die("MLton_Process_cwait not implemented");
}
