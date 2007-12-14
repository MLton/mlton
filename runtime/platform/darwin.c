#include <mach-o/getsect.h>  // for get_etext()
#include <dlfcn.h>
#include <stdio.h>

#include "platform.h"

#include "diskBack.unix.c"
#include "mkdir2.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "sysctl.c"
#include "use-mmap.c"

code_pointer GC_getTextEnd (void) {
        return (code_pointer)(long)(get_etext ());
}

code_pointer GC_getTextStart (void) {
        void* address;
        Dl_info info;
        
        address = dlsym(RTLD_DEFAULT, "main");
        dladdr(address, &info);
        
        return (code_pointer)info.dli_fbase;
}

void GC_displayMem (void) {
        static char buffer[256];

        snprintf (buffer, cardof(buffer), "/usr/bin/vmmap -w -interleaved %d\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int sig,  
                     __attribute__ ((unused)) siginfo_t *sip, 
                     ucontext_t *ucp) {
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
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
