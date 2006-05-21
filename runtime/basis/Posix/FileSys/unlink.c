#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_unlink (NullString8_t p) {
  return unlink ((const char*) p);
}
