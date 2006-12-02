#include "platform.h"

C_Errno_t(C_String_t) Posix_ProcEnv_ttyname (C_Fd_t f) {
  char *res = ttyname (f);
  return (C_Errno_t(C_String_t))res;
}
