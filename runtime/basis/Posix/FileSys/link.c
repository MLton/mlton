#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_link (NullString8_t p1, NullString8_t p2) {
  return link ((const char *) p1, (const char *) p2);
}
