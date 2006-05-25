#include "platform.h"

#include "diskBack.unix.c"
#include "getText.c"
#include "mkdir2.c"
#include "displayMem.linux.c"
#include "mmap-protect.c"
#include "sysctl.c"
#include "use-mmap.c"

static void catcher (__attribute__ ((unused)) int sig,
                     __attribute__ ((unused)) int code, 
                     struct sigcontext *ucp) {
        GC_handleSigProf ((code_pointer) ucp->sc_eip);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART;
        sa->sa_handler = (void (*)(int))catcher;
}
