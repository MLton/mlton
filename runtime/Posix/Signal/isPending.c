#include <signal.h>
#include "gc.h"
#include "mlton-posix.h"

extern struct GC_state gcState;

bool Posix_Signal_isPending(Int signum) {
      return sigismember(&gcState.signalsPending, signum);
}
