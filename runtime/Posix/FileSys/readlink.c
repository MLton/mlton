#include "platform.h"

C_Errno_t(C_SSize_t) Posix_FileSys_readlink (NullString8_t p, Array(Char8_t) b, C_Size_t n) {
  return readlink ((const char*)p, (char*)b, n);
}
