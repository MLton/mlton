#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_ftruncate (C_Fd_t f, C_Off_t n) {
  return ftruncate (f, n);
}
