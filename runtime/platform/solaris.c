#include "platform.h"

#include <ieeefp.h>

#include "platform/diskBack.unix.c"
#include "platform/float-math.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysconf.c"
#include "platform/setenv.putenv.c"
#include "platform/use-mmap.c"

#ifdef __sparc__
int fegetround (void) {
        int mode;

        mode = fpgetround ();
        switch (mode) {
        case FP_RN: mode = 0; break;
        case FP_RM: mode = 1; break;
        case FP_RP: mode = 2; break;
        case FP_RZ: mode = 3; break;
        default:
                die ("fegetround: invalid mode %d\n", mode);
        }
        return mode;
}

int fesetround (int mode) {
        switch (mode) {
        case 0: mode = FP_RN; break;
        case 1: mode = FP_RM; break;
        case 2: mode = FP_RP; break;
        case 3: mode = FP_RZ; break;
        default:
                die ("fesetround: invalid mode %d\n", mode);
        }
        fpsetround (mode);
        return 0;
}
#endif /* __sparc__ */

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
