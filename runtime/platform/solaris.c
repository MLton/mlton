#include "platform.h"

#include <ieeefp.h>

#include "diskBack.unix.c"
#include "float-math.c"
#include "mkdir2.c"
#include "mmap.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "sysconf.c"
#include "setenv.putenv.c"

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

int fpclassify64 (double d) {
        fpclass_t c;

        c = fpclass (d);
        switch (c) {
        case FP_SNAN:
        case FP_QNAN:
                return FP_NAN;
        case FP_NINF:
        case FP_PINF:
                return FP_INFINITE;
        case FP_NDENORM:
        case FP_PDENORM:
                return FP_SUBNORMAL;
        case FP_NZERO:
        case FP_PZERO:
                return FP_ZERO;
        case FP_NNORM:
        case FP_PNORM:
                return FP_NORMAL;
        default:
                die ("Real_class error: invalid class %d\n", c);
        }
}

/* ------------------------------------------------- */
/*                        GC                         */
/* ------------------------------------------------- */

void GC_displayMem (void) {
        static char buffer[256];
        snprintf (buffer, cardof(buffer), "pmap %d\n", (int)(getpid ()));
        system (buffer);
}

static void catcher (__attribute__ ((unused)) int sig,
                     __attribute__ ((unused)) siginfo_t *sip,
                     ucontext_t *ucp) {
        GC_handleSigProf ((code_pointer) ucp->uc_mcontext.gregs[REG_PC]);
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

/* On Solaris 5.7, MAP_ANON causes EINVAL and mmap requires a file descriptor.
 */
void *GC_mmapAnon (void *start, size_t length) {
        static int fd = -1;

        if (-1 == fd)
                fd = open ("/dev/zero", O_RDONLY);
        return mmap (start, length, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
}

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}
