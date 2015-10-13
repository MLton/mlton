#include "platform.h"

extern struct GC_state gcState;

static void handler (int signum) {
  GC_handler (&gcState, signum);
}

C_Errno_t(C_Int_t) Posix_Signal_default (C_Signal_t signum) {
  struct sigaction sa;

  sigdelset (GC_getSignalsHandledAddr (&gcState), signum);
  memset (&sa, 0, sizeof(sa));
  sa.sa_handler = SIG_DFL;
  return sigaction (signum, &sa, NULL);
}

C_Errno_t(C_Int_t) Posix_Signal_isDefault (C_Int_t signum, Ref(C_Int_t) isDef) {
  int res;
  struct sigaction sa;

  memset (&sa, 0, sizeof(sa));
  res = sigaction (signum, NULL, &sa);
  *((C_Int_t*)isDef) = sa.sa_handler == SIG_DFL;
  return res;
}

C_Errno_t(C_Int_t) Posix_Signal_ignore (C_Signal_t signum) {
  struct sigaction sa;

  sigdelset (GC_getSignalsHandledAddr (&gcState), signum);
  memset (&sa, 0, sizeof(sa));
  sa.sa_handler = SIG_IGN;
  return sigaction (signum, &sa, NULL);
}

C_Errno_t(C_Int_t) Posix_Signal_isIgnore (C_Int_t signum, Ref(C_Int_t) isIgn) {
  int res;
  struct sigaction sa;

  memset (&sa, 0, sizeof(sa));
  res = sigaction (signum, NULL, &sa);
  *((C_Int_t*)isIgn) = sa.sa_handler == SIG_IGN;
  return res;
}

C_Errno_t(C_Int_t) Posix_Signal_handlee (C_Int_t signum) {
  struct sigaction sa;

  sigaddset (GC_getSignalsHandledAddr (&gcState), signum);
  memset (&sa, 0, sizeof(sa));
  /* The mask must be full because GC_handler reads and writes 
   * s->signalsPending (else there is a race condition).
   */
  sigfillset (&sa.sa_mask);
#if HAS_SIGALTSTACK
  sa.sa_flags = SA_ONSTACK;
#endif
  sa.sa_handler = handler;
  return sigaction (signum, &sa, NULL);
}

void Posix_Signal_handleGC (void) {
  GC_setGCSignalHandled (&gcState, TRUE);
}

C_Int_t Posix_Signal_isPending (C_Int_t signum) {
  return sigismember (GC_getSignalsPendingAddr (&gcState), signum);
}

C_Int_t Posix_Signal_isPendingGC (void) {
  return GC_getGCSignalPending (&gcState);
}

void Posix_Signal_resetPending (void) {
  sigemptyset (GC_getSignalsPendingAddr (&gcState));
  GC_setGCSignalPending (&gcState, FALSE);
}

C_Errno_t(C_Int_t) Posix_Signal_sigaddset (Array(Word8_t) sigset, C_Signal_t signum) {
  return sigaddset ((sigset_t*)sigset, signum);
}

C_Errno_t(C_Int_t) Posix_Signal_sigdelset (Array(Word8_t) sigset, C_Signal_t signum) {
  return sigdelset ((sigset_t*)sigset, signum);
}

C_Errno_t(C_Int_t) Posix_Signal_sigemptyset (Array(Word8_t) sigset) {
  return sigemptyset ((sigset_t*)sigset);
}

C_Errno_t(C_Int_t) Posix_Signal_sigfillset (Array(Word8_t) sigset) {
  return sigfillset ((sigset_t*)sigset);
}

C_Errno_t(C_Int_t) Posix_Signal_sigismember (Vector(Word8_t) sigset, C_Signal_t signum) {
  return sigismember ((sigset_t*)sigset, signum);
}

C_Errno_t(C_Int_t) Posix_Signal_sigprocmask (C_Int_t how, Vector(Word8_t) sigset, Array(Word8_t) oldsigset) {
  return sigprocmask (how, (sigset_t*)sigset, (sigset_t*)oldsigset);
}

#if ASSERT
#define LOCAL_USED_FOR_ASSERT
#else
#define LOCAL_USED_FOR_ASSERT  __attribute__ ((unused))
#endif

void Posix_Signal_sigsuspend (Vector(Word8_t) sigset) {
  LOCAL_USED_FOR_ASSERT int res;

  res = sigsuspend ((sigset_t*)sigset);
  assert (-1 == res);
}
