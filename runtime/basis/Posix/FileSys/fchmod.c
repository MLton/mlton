#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_fchmod (C_Fd_t f, C_Mode_t m) {
  return fchmod (f, m);
}
