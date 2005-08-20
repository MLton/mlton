#include "platform.h"

#include "getrusage.c"
#include "getText.c"
#include "mkdir2.c"
#include "showMem.linux.c"
#include "signbit.c"
#include "ssmmap.c"
#include "totalRam.sysctl.c"
#include "use-mmap.c"

static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
        GC_handleSigProf ((pointer) ucp->sc_eip);
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
