#include <mach-o/dyld.h>
#include <mach-o/getsect.h>  // for get_etext()
#include <stdio.h>

#include "platform.h"

#include "mkdir2.c"
#include "mmap-protect.c"
#include "use-mmap.c"

code_pointer GC_getTextEnd (void) {
        return (code_pointer)(long)(get_etext ());
}

code_pointer GC_getTextStart (void) {
        void *address;
        const struct mach_header *mh;

        _dyld_lookup_and_bind ("_main", &address, 0);
        mh = _dyld_get_image_header_containing_address (address);
        return (code_pointer)mh;
}

void GC_displayMem (void) {
        static char buffer[256];

        sprintf (buffer, "/usr/bin/vmmap -w -interleaved %d\n", (int)getpid ());
        (void)system (buffer);
}

static void catcher (__attribute__ ((unused)) int sig,  
                     __attribute__ ((unused)) siginfo_t *sip, 
                     ucontext_t *ucp) {
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext->ss.srr0);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

size_t GC_pageSize (void) {
        long int pageSize = sysconf(_SC_PAGESIZE);
        return (size_t)pageSize;
}

size_t GC_totalRam (void) {
        int mem;
        size_t len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return mem;
}

// Not quite right... what does 'available' mean anyways?!
// This returns the number of bytes available to userspace programs.
size_t GC_availRam (void) {
        int mem;
        size_t len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.usermem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return mem;
}
