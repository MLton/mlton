#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/displayMem.proc.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysctl.c"
#include "platform/use-mmap.c"

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.__gregs[_REG_EIP]);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
