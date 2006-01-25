#include "platform.h"

void Posix_Error_clearErrno (void) {
  errno = 0;
}

C_Int_t Posix_Error_getErrno (void) {
  return errno;
}

C_String_t Posix_Error_strError (C_Int_t n) {
  char *res = strerror (n);
  return (C_String_t)res;
}
