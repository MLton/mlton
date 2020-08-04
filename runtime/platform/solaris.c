#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysconf.c"
#include "platform/setenv.putenv.c"
#include "platform/use-mmap.c"

/* ------------------------------------------------- */
/*                        GC                         */
/* ------------------------------------------------- */

void GC_displayMem (void) {
        static char buffer[256];
        snprintf (buffer, cardof(buffer), "pmap %d\n", (int)(getpid ()));
        system (buffer);
}

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[REG_PC]);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
