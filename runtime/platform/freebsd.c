#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysctl.c"
#include "platform/use-mmap.c"

void GC_displayMem (void) {
        static char buffer[256];

        snprintf (buffer, cardof(buffer), "/bin/cat /proc/%d/map\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
#if (defined (__x86_64__))
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.mc_rip);
#elif (defined (__i386__))
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.mc_eip);
#elif (defined (__arm__))
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.__gregs[_REG_PC]);
#elif (defined (__aarch64__))
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.mc_gpregs.gp_elr);
#else
#error Profiling handler is missing for this architecture
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
