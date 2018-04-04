#define _GNU_SOURCE

#include "platform.h"

#include "diskBack.unix.c"
#include "displayMem.proc.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "use-mmap.c"

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
#if (defined (__x86_64__))
#ifndef REG_RIP
#define REG_INDEX(NAME) (offsetof(struct sigcontext, NAME) / sizeof(greg_t))
#define REG_RIP REG_INDEX(rip) /* seems to be 16 */
#endif
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[REG_RIP]);
#elif (defined (__alpha__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) (ucp->uc_mcontext.sc_pc));
#elif (defined (__hppa__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) (ucp->uc_mcontext.sc_iaoq[0] & ~0x3UL));
#elif (defined(__ia64__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->_u._mc.sc_ip);
#elif (defined (__ppc__)) || (defined (__powerpc__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.regs->nip);
#elif (defined (__sparc__))
        struct sigcontext* scp = (struct sigcontext*)context;
#if __WORDSIZE == 64
        GC_handleSigProf ((code_pointer) scp->sigc_regs.tpc);
#else
        GC_handleSigProf ((code_pointer) scp->si_regs.pc);
#endif
#elif (defined (__mips__))
        ucontext_t* ucp = (ucontext_t*)context;
#ifdef __UCLIBC__
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gpregs[CTX_EPC]);
#else
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.pc);
#endif
#elif (defined (__i386__))
#ifndef EIP
#define EIP     14
#endif
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[EIP]);
#elif (defined (__arm__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.arm_pc);
#elif (defined (__aarch64__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.pc);
#elif (defined (__s390__))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.psw.addr);
#elif (defined (__riscv))
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.__gregs[REG_PC]);
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

size_t GC_pageSize (void) {
        long int pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

/* sysconf(_SC_PHYS_PAGES) is not portable (mipsel uclibc) */
uintmax_t GC_physMem (void) {
        struct sysinfo si;
        if (sysinfo(&si) < 0)
                diee ("GC_physMem error: sysinfo failed");
        
        return (uintmax_t)si.totalram * (uintmax_t)si.mem_unit;
}
