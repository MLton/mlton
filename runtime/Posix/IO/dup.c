#include "platform.h"

C_Errno_t(C_Fd_t) Posix_IO_dup (C_Fd_t f) {
  return dup (f);
}
