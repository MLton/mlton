#include "platform.h"

#include "diskBack.unix.c"
#include "getText.c"
#include "mkdir2.c"
#include "mmap-protect.c"
#include "use-mmap.c"

void GC_displayMem () {
        static char buffer[256];

        sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int sig,
                     __attribute__ ((unused)) siginfo_t *sip, 
                     ucontext_t *ucp) {
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.mc_eip);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

size_t GC_totalRam (void) {
        unsigned int mem;
        size_t len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return (size_t)mem;
}
