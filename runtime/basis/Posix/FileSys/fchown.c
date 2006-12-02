#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_fchown (C_Fd_t f, C_UId_t u, C_GId_t g) {
  return fchown (f, u, g);
}
