#include "platform.h"

#include <dlfcn.h>
#include <stdio.h>

#include "platform/diskBack.unix.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysctl.c"
#include "platform/use-mmap.c"

void GC_displayMem (void) {
        static char buffer[256];

        snprintf (buffer, cardof(buffer), "/usr/bin/vmmap -w -interleaved %d\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
#if (defined(__powerpc__) || defined(__ppc__))
#if __DARWIN_UNIX03
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->__ss.__srr0);
#else
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.srr0);
#endif
#elif (defined(__i386__))
#if __DARWIN_UNIX03
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->__ss.__eip);
#else
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.eip);
#endif
#elif (defined(__x86_64__))
#if __DARWIN_UNIX03
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->__ss.__rip);
#else
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.rip);
#endif
#else
#error Unsupported darwin CPU architecture
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
