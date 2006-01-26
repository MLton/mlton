#include "platform.h"

C_Errno_t(C_Int_t) Posix_IO_fcntl2 (C_Fd_t f, C_Int_t i) {
  return fcntl (f, i);
}
