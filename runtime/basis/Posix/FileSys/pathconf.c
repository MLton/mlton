#include "platform.h"

C_Errno_t(C_Long_t) Posix_FileSys_pathconf (NullString8_t p, C_Int_t n) {
  return pathconf ((const char *)p, n);
}
