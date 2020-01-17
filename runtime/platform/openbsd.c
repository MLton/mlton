#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/displayMem.proc.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysctl.c"
#include "platform/mmap.c"

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        return mmapAnonFlags (start, length, MAP_STACK);
}

void *GC_mmapAnonFlags (void *start, size_t length, int flags) {
	return mmapAnonFlags (start, length, flags | MAP_STACK);
}

void *GC_mmapAnonStack (void *start, size_t length, int prot,
                        size_t dead_low, size_t dead_high) {
	int flags = 0;
#ifdef MAP_STACK
	flags |= MAP_STACK;
#endif
	return GC_mmapAnonFlags_safe_protect (start, length, prot, flags,
	                                   dead_low, dead_high);
}

static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;

#if (defined (__x86_64__))
        GC_handleSigProf ((code_pointer) ucp->sc_rip);
#elif (defined (__i386__))
        GC_handleSigProf ((code_pointer) ucp->sc_eip);
#else
#error Profiling handler is missing for this architecture
#endif

}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}
