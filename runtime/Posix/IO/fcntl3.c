#include "platform.h"

C_Errno_t(C_Int_t) Posix_IO_fcntl3 (C_Fd_t f, C_Int_t i, C_Int_t j) {
  return fcntl (f, i, j);
}
