#include "platform.h"

#include "getrusage.c"
#include "getText.c"
#include "mkdir2.c"
#include "showMem.linux.c"
#include "ssmmap.c"
#include "totalRam.sysctl.c"
#include "use-mmap.c"

static void catcher (int sig, int code, struct sigcontext *ucp) {
        GC_handleSigProf ((pointer) ucp->sc_eip);
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART;
        sa->sa_handler = (void (*)(int))catcher;
}
