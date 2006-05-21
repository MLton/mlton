#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_chdir(NullString8_t p) {
  return chdir((const char *) p);
}
