#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_mkdir (NullString8_t p, C_Mode_t m) {
  return mkdir ((const char *) p, m);
}
