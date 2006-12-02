#include "platform.h"

C_Errno_t(C_Fd_t) Posix_FileSys_open2 (NullString8_t p, C_Int_t i) {
  return open ((const char*)p, i);
}
