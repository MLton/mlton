#include "platform.h"

C_Errno_t(C_Fd_t) Posix_FileSys_open3 (NullString8_t p, C_Int_t i, C_Mode_t m) {
  return open ((const char*)p, i, (mode_t)m);
}
