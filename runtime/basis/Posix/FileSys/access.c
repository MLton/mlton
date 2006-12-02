#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_access (NullString8_t f, C_Int_t i) {
  return access ((const char *) f, i);
}
