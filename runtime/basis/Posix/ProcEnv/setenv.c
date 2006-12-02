#include "platform.h"

C_Errno_t(C_Int_t) Posix_ProcEnv_setenv (NullString8_t s, NullString8_t v) {
  return setenv ((const char*)s, (const char*)v, 1);
}
