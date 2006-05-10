#include <mach-o/dyld.h>
#include <mach-o/getsect.h>  // for get_etext()
#include <stdio.h>

#include "platform.h"

#include "mkdir2.c"
#include "mmap-protect.c"
#include "use-mmap.c"

const void *GC_getTextEnd (void) {
        return (const void*)(long)(get_etext ());
}

const void *GC_getTextStart (void) {
        void *address;
        const struct mach_header *mh;

        _dyld_lookup_and_bind ("_main", &address, 0);
        mh = _dyld_get_image_header_containing_address (address);
        return mh;
}

void GC_displayMem (void) {
        /* FIXME: this won't actually work. */
        static char buffer[256];

        sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int sig,  
                        __attribute__ ((unused)) siginfo_t *sip, 
                        ucontext_t *ucp) {
        GC_handleSigProf ((pointer) ucp->uc_mcontext->ss.srr0);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

size_t GC_totalRam (void) {
        int mem;
        size_t len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return mem;
}
