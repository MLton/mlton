#include "platform.h"

C_Errno_t(C_PId_t) Posix_Process_fork (void) {
  pid_t pid = fork ();
#if (defined (__Darwin__))
  /* Contrary to the documentation, a forked process does not inherit
   * the alternate signal stack; re-install the alternate signal
   * stack.
   */
  if (pid == 0) {
    GC_initSignalStack();
  }
#endif
  return pid;
}
