#include "platform.h"

C_Errno_t(C_Fd_t) Posix_IO_dup2 (C_Fd_t f1, C_Fd_t f2) {
  return dup2 (f1, f2);
}
