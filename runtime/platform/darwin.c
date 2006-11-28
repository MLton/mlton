#include <mach-o/dyld.h>
#include <mach-o/getsect.h>  // for get_etext()
#include <stdio.h>

#include "platform.h"

#include "getrusage.c"
#include "mkdir2.c"
#include "ssmmap.c"
#include "use-mmap.c"

static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
#if (defined(__powerpc__) || defined(__ppc__))
        GC_handleSigProf ((pointer) ucp->uc_mcontext->ss.srr0);
#elif (defined(__i386__))
        GC_handleSigProf ((pointer) ucp->uc_mcontext->ss.eip);
#endif
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

void *getTextEnd () {
        return (void*)(get_etext ());
}

void *getTextStart () {
        void *address;
        void *module;
        const struct mach_header *mh;

        _dyld_lookup_and_bind ("_main", &address, (NSModule*)&module);
        mh = _dyld_get_image_header_containing_address (address);
        return (void*)mh;
}

void showMem () {
        /* FIXME: this won't actually work. */
        static char buffer[256];

        sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
        (void)system (buffer);
}

W32 totalRam (GC_state s) {
        int mem;
        size_t len;

        len = sizeof (int);
        if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
                diee ("sysctl failed");
        return mem;
}
