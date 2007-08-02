#include <mach-o/dyld.h>
#include <mach-o/getsect.h>  // for get_etext()
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
        void *address;
        const struct mach_header *mh;

        _dyld_lookup_and_bind ("_main", &address, NULL);
        mh = _dyld_get_image_header_containing_address (address);
        return (code_pointer)mh;
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
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.srr0);
#elif (defined(__i386__))
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.eip);
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
