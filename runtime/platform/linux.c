#define _GNU_SOURCE

#include "platform.h"

#include "diskBack.unix.c"
#include "mkdir2.c"
#include "displayMem.linux.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "sysconf.c"
#include "use-mmap.c"
#ifdef __UCLIBC__
#include "feround.c"
#endif

#ifndef EIP
#define EIP     14
#endif

/* potentially correct for other archs:
 *  alpha: ucp->m_context.sc_pc
 *  arm: ucp->m_context.ctx.arm_pc
 *  ia64: ucp->m_context.sc_ip & ~0x3UL
 *  s390: ucp->m_context.sregs->regs.psw.addr
 */
static void catcher (__attribute__ ((unused)) int sig, 
                     __attribute__ ((unused)) siginfo_t* sip, 
                     void* mystery) {
#if (defined (__x86_64__))
#define REG_INDEX(NAME) (offsetof(struct sigcontext, NAME) / sizeof(greg_t))
#ifndef REG_RIP
#define REG_RIP REG_INDEX(rip) /* seems to be 16 */
#endif
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[REG_RIP]);
#elif (defined (__hppa__))
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((code_pointer) (ucp->uc_mcontext.sc_iaoq[0] & ~0x3UL));
#elif (defined (__ppc__)) || (defined (__powerpc__))
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.regs->nip);
#elif (defined (__sparc__))
        struct sigcontext* scp = (struct sigcontext*)mystery;
#if __WORDSIZE == 64
        GC_handleSigProf ((code_pointer) scp->sigc_regs.tpc);
#else
        GC_handleSigProf ((code_pointer) scp->si_regs.pc);
#endif
#elif (defined (__mips__))
        ucontext_t* ucp = (ucontext_t*)mystery;
#ifdef __UCLIBC__
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gpregs[CTX_EPC]);
#else
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.pc);
#endif
#elif (defined (__i386__))
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[EIP]);
#else
#error Profiling handler is missing for this architecture
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

/* We need the value of MREMAP_MAYMOVE, which should come from sys/mman.h, but
 * isn't there.  It is in linux/mman.h, but we can't #include that here, because
 * kernel headers don't mix with system headers.  We could create a separate
 * file, include the kernel headers there, and define a global.  But there
 * sometimes seem to be problems including kernel headers, so the easiest thing
 * to do is just define MREMAP_MAYMOVE.
 */
#define MREMAP_MAYMOVE 1

void *GC_mremap (void *start, size_t oldLength, size_t newLength) {
        return mremap (start, oldLength, newLength, MREMAP_MAYMOVE);
}

