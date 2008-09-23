#include "platform.h"

#include "diskBack.unix.c"
#include "mkdir2.c"
#include "displayMem.linux.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "sysctl.c"
#include "use-mmap.c"

static void catcher (__attribute__ ((unused)) int sig,
                     __attribute__ ((unused)) siginfo_t *sip,
                     ucontext_t *ucp) {
        GC_handleSigProf ((code_pointer) ucp->sc_eip);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
