#include "platform.h"

#if (defined (NSIG))
const C_Int_t Posix_Signal_NSIG = NSIG;
#elif (defined (_NSIG))
const C_Int_t Posix_Signal_NSIG = _NSIG;
#else
#error Posix_Signal_numSignals not defined
#endif

const C_Int_t Posix_Signal_SIG_BLOCK = SIG_BLOCK;
const C_Int_t Posix_Signal_SIG_SETMASK = SIG_SETMASK;
const C_Int_t Posix_Signal_SIG_UNBLOCK = SIG_UNBLOCK;

const C_Signal_t Posix_Signal_SIGABRT = SIGABRT;
const C_Signal_t Posix_Signal_SIGALRM = SIGALRM;
const C_Signal_t Posix_Signal_SIGBUS = SIGBUS;
const C_Signal_t Posix_Signal_SIGCHLD = SIGCHLD;
const C_Signal_t Posix_Signal_SIGCONT = SIGCONT;
const C_Signal_t Posix_Signal_SIGFPE = SIGFPE;
const C_Signal_t Posix_Signal_SIGHUP = SIGHUP;
const C_Signal_t Posix_Signal_SIGILL = SIGILL;
const C_Signal_t Posix_Signal_SIGINT = SIGINT;
const C_Signal_t Posix_Signal_SIGKILL = SIGKILL;
const C_Signal_t Posix_Signal_SIGPIPE = SIGPIPE;
const C_Signal_t Posix_Signal_SIGQUIT = SIGQUIT;
const C_Signal_t Posix_Signal_SIGSEGV = SIGSEGV;
const C_Signal_t Posix_Signal_SIGSTOP = SIGSTOP;
const C_Signal_t Posix_Signal_SIGTERM = SIGTERM;
const C_Signal_t Posix_Signal_SIGTSTP = SIGTSTP;
const C_Signal_t Posix_Signal_SIGTTIN = SIGTTIN;
const C_Signal_t Posix_Signal_SIGTTOU = SIGTTOU;
const C_Signal_t Posix_Signal_SIGUSR1 = SIGUSR1;
const C_Signal_t Posix_Signal_SIGUSR2 = SIGUSR2;
const C_Signal_t Posix_Signal_SIGPOLL = SIGPOLL;
const C_Signal_t Posix_Signal_SIGPROF = SIGPROF;
const C_Signal_t Posix_Signal_SIGSYS = SIGSYS;
const C_Signal_t Posix_Signal_SIGTRAP = SIGTRAP;
const C_Signal_t Posix_Signal_SIGURG = SIGURG;
const C_Signal_t Posix_Signal_SIGVTALRM = SIGVTALRM;
const C_Signal_t Posix_Signal_SIGXCPU = SIGXCPU;
const C_Signal_t Posix_Signal_SIGXFSZ = SIGXFSZ;
