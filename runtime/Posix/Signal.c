#include "platform.h"

extern struct GC_state gcState;

static void handler (int signum) {
        GC_handler (&gcState, signum);
}

enum {
#if  (defined (SA_ONSTACK))
        SA_FLAGS = SA_ONSTACK,
#else
        SA_FLAGS = 0,
#endif
};

Int Posix_Signal_default (Int signum) {
        struct sigaction sa;

        sigdelset (GC_getSignalsHandledAddr (&gcState), signum);
        memset (&sa, 0, sizeof(sa));
        sa.sa_handler = SIG_DFL;
        sa.sa_flags = SA_FLAGS;
        return sigaction (signum, &sa, NULL);
}

bool Posix_Signal_isGCPending () {
        Bool res;

        res = GC_getGCSignalPending (&gcState);
        if (DEBUG_SIGNALS)
                fprintf (stderr, "%s = Posix_Signal_isGCPending ()\n",
                                boolToString (res));
        return res;
}

Bool Posix_Signal_isPending (Int signum) {
        return sigismember (GC_getSignalsPendingAddr (&gcState), signum);
}

Int Posix_Signal_handle (Int signum) {
        static struct sigaction sa;

        sigaddset (GC_getSignalsHandledAddr (&gcState), signum);
        memset (&sa, 0, sizeof(sa));
        /* The mask must be full because GC_handler reads and writes 
         * s->signalsPending  (else there is a race condition).
         */
        sigfillset (&sa.sa_mask);
        sa.sa_handler = handler;
        sa.sa_flags = SA_FLAGS;
        return sigaction (signum, &sa, NULL);
}

void Posix_Signal_handleGC () {
        GC_setGCSignalHandled (&gcState, TRUE);
}

Int Posix_Signal_ignore (Int signum) {
        struct sigaction sa;

        sigdelset (GC_getSignalsHandledAddr (&gcState), signum);
        memset (&sa, 0, sizeof(sa));
        sa.sa_handler = SIG_IGN;
        sa.sa_flags = SA_FLAGS;
        return sigaction (signum, &sa, NULL);
}

Int Posix_Signal_isDefault (Int signum, Bool *isDef) {
        Int res;
        struct sigaction sa;

        sa.sa_flags = SA_FLAGS;
        res = sigaction (signum, NULL, &sa);
        *isDef = sa.sa_handler == SIG_DFL;
        return res;
}

void Posix_Signal_resetPending () {
        if (DEBUG_SIGNALS)
                fprintf (stderr, "Posix_Signal_resetPending ()\n");
        sigemptyset (GC_getSignalsPendingAddr (&gcState));
        GC_setGCSignalPending (&gcState, FALSE);
}

static sigset_t set;

Int Posix_Signal_sigaddset (Int signum) {
        return sigaddset (&set, signum);
}

Int Posix_Signal_sigdelset (Int signum) {
        return sigdelset (&set, signum);
}

Int Posix_Signal_sigemptyset () {
        return sigemptyset (&set);
}

Int Posix_Signal_sigfillset () {
        return sigfillset (&set);
}

Int Posix_Signal_sigismember (Int signum) {
        return sigismember (&set, signum);
}

Int Posix_Signal_sigprocmask (Int how) {
        return sigprocmask (how, &set, &set);
}

void Posix_Signal_suspend () {
        int res;

        res = sigsuspend (&set);
        assert (-1 == res);
}
