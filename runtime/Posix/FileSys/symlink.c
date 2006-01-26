#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_symlink (NullString8_t p1, NullString8_t p2) {
  return symlink ((const char*) p1, (const char*) p2);
}
