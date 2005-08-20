#include "platform.h"

#include "getrusage.c"
#include "getText.c"
#include "mkdir2.c"
#include "ssmmap.c"
#include "use-mmap.c"

static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
        GC_handleSigProf ((pointer) ucp->uc_mcontext.mc_eip);
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

void showMem () {
        static char buffer[256];

        sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
        (void)system (buffer);
}

W32 totalRam (GC_state s) {
        int mem, len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return mem;
}
