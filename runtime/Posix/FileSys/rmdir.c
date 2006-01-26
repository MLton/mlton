#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_rmdir (NullString8_t p) {
  return rmdir ((const char*) p);
}
