#include "platform.h"

C_Errno_t(C_Off_t) Posix_IO_lseek (C_Fd_t f, C_Off_t i, C_Int_t j) {
  return lseek (f, i, j);
}
