#include "platform.h"

C_String_t Posix_ProcEnv_getenv (NullString8_t s) {
  char *res = getenv((const char*)s);
  return (C_String_t)res;
}
