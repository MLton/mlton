#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_truncate (NullString8_t p, C_Off_t n) {
  return truncate ((const char*)p, n);
}
