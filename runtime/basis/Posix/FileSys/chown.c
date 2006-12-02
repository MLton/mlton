#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_chown (NullString8_t p, C_UId_t u, C_GId_t g) {
  return chown ((const char *) p, u, g);
}
