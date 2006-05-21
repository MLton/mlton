#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_fchdir(C_Fd_t f) {
  return fchdir(f);
}
