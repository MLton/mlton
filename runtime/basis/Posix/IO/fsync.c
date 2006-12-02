#include "platform.h"

C_Errno_t(C_Int_t) Posix_IO_fsync (C_Fd_t f) {
  return fsync (f);
}
