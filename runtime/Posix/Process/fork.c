#include "platform.h"

C_Errno_t(C_PId_t) Posix_Process_fork (void) {
  return fork ();
}
