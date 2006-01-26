#include "platform.h"

C_String_t Posix_FileSys_getcwd (Array(Char8_t) buf, C_Size_t n) {
  char *res = getcwd ((char*)buf, n);
  return (C_String_t)res;
}
