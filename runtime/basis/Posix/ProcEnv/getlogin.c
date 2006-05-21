#include "platform.h"

C_Errno_t(C_String_t) Posix_ProcEnv_getlogin (void) {
  char *res = getlogin ();
  return (C_Errno_t(C_String_t))res;
}
