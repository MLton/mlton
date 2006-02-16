#include "platform.h"

#include <ieeefp.h>

#include "getrusage.c"
#include "getText.c"
#include "mkdir2.c"
#include "mmap.c"
#include "signbit.c"
#include "ssmmap.c"
#include "totalRam.sysconf.c"

static void catcher (int sig, siginfo_t *sip, ucontext_t *ucp) {
        GC_handleSigProf ((pointer) ucp->uc_mcontext.gregs[REG_PC]);
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

void decommit (void *base, size_t length) {
        smunmap (base, length);
}

int fegetround () {
        int mode;

        mode = fpgetround ();
        switch (mode) {
        case FP_RN: mode = 0; break;
        case FP_RM: mode = 1; break;
        case FP_RP: mode = 2; break;
        case FP_RZ: mode = 3; break;
        }
        return mode;
}

void fesetround (int mode) {
        switch (mode) {
        case 0: mode = FP_RN; break;
        case 1: mode = FP_RM; break;
        case 2: mode = FP_RP; break;
        case 3: mode = FP_RZ; break;
        }
        fpsetround (mode);
}

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
 
/* On Solaris 5.7, MAP_ANON causes EINVAL and mmap requires a file descriptor.
 */
void *mmapAnon (void *start, size_t length) {
        static int fd = -1;

        if (-1 == fd)
                fd = open ("/dev/zero", O_RDONLY);
        return mmap (start, length, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
}

void release (void *base, size_t length) {
        smunmap (base, length);
}

/* This implementation of setenv has a space leak, but I don't see how to avoid 
 * it, since the specification of putenv is that it uses the memory for its arg.
 */
int setenv (const char *name, const char *value, int overwrite) {
        char *b;

        b = malloc (strlen (name) + strlen (value) + 2 /* = and \000 */);
        sprintf (b, "%s=%s", name, value);
        return putenv (b);
}

void showMem () {
        static char buffer[256];
        sprintf (buffer, "pmap %d\n", (int)(getpid ()));
        system (buffer);
}
