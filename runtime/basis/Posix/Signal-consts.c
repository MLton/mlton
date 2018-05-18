#include "platform.h"

const C_Size_t Posix_Signal_sigSetLen = sizeof (sigset_t);

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

#ifndef SIGABRT
#define SIGABRT -1
#endif
const C_Signal_t Posix_Signal_SIGABRT = SIGABRT;
#ifndef SIGALRM
#define SIGALRM -1
#endif
const C_Signal_t Posix_Signal_SIGALRM = SIGALRM;
#ifndef SIGBUS
#define SIGBUS -1
#endif
const C_Signal_t Posix_Signal_SIGBUS = SIGBUS;
#ifndef SIGCHLD
#define SIGCHLD -1
#endif
const C_Signal_t Posix_Signal_SIGCHLD = SIGCHLD;
#ifndef SIGCONT
#define SIGCONT -1
#endif
const C_Signal_t Posix_Signal_SIGCONT = SIGCONT;
#ifndef SIGFPE
#define SIGFPE -1
#endif
const C_Signal_t Posix_Signal_SIGFPE = SIGFPE;
#ifndef SIGHUP
#define SIGHUP -1
#endif
const C_Signal_t Posix_Signal_SIGHUP = SIGHUP;
#ifndef SIGILL
#define SIGILL -1
#endif
const C_Signal_t Posix_Signal_SIGILL = SIGILL;
#ifndef SIGINT
#define SIGINT -1
#endif
const C_Signal_t Posix_Signal_SIGINT = SIGINT;
#ifndef SIGKILL
#define SIGKILL -1
#endif
const C_Signal_t Posix_Signal_SIGKILL = SIGKILL;
#ifndef SIGPIPE
#define SIGPIPE -1
#endif
const C_Signal_t Posix_Signal_SIGPIPE = SIGPIPE;
#ifndef SIGQUIT
#define SIGQUIT -1
#endif
const C_Signal_t Posix_Signal_SIGQUIT = SIGQUIT;
#ifndef SIGSEGV
#define SIGSEGV -1
#endif
const C_Signal_t Posix_Signal_SIGSEGV = SIGSEGV;
#ifndef SIGSTOP
#define SIGSTOP -1
#endif
const C_Signal_t Posix_Signal_SIGSTOP = SIGSTOP;
#ifndef SIGTERM
#define SIGTERM -1
#endif
const C_Signal_t Posix_Signal_SIGTERM = SIGTERM;
#ifndef SIGTSTP
#define SIGTSTP -1
#endif
const C_Signal_t Posix_Signal_SIGTSTP = SIGTSTP;
#ifndef SIGTTIN
#define SIGTTIN -1
#endif
const C_Signal_t Posix_Signal_SIGTTIN = SIGTTIN;
#ifndef SIGTTOU
#define SIGTTOU -1
#endif
const C_Signal_t Posix_Signal_SIGTTOU = SIGTTOU;
#ifndef SIGUSR1
#define SIGUSR1 -1
#endif
const C_Signal_t Posix_Signal_SIGUSR1 = SIGUSR1;
#ifndef SIGUSR2
#define SIGUSR2 -1
#endif
const C_Signal_t Posix_Signal_SIGUSR2 = SIGUSR2;
#ifndef SIGPOLL
#define SIGPOLL -1
#endif
const C_Signal_t Posix_Signal_SIGPOLL = SIGPOLL;
#ifndef SIGPROF
#define SIGPROF -1
#endif
const C_Signal_t Posix_Signal_SIGPROF = SIGPROF;
#ifndef SIGSYS
#define SIGSYS -1
#endif
const C_Signal_t Posix_Signal_SIGSYS = SIGSYS;
#ifndef SIGTRAP
#define SIGTRAP -1
#endif
const C_Signal_t Posix_Signal_SIGTRAP = SIGTRAP;
#ifndef SIGURG
#define SIGURG -1
#endif
const C_Signal_t Posix_Signal_SIGURG = SIGURG;
#ifndef SIGVTALRM
#define SIGVTALRM -1
#endif
const C_Signal_t Posix_Signal_SIGVTALRM = SIGVTALRM;
#ifndef SIGXCPU
#define SIGXCPU -1
#endif
const C_Signal_t Posix_Signal_SIGXCPU = SIGXCPU;
#ifndef SIGXFSZ
#define SIGXFSZ -1
#endif
const C_Signal_t Posix_Signal_SIGXFSZ = SIGXFSZ;
