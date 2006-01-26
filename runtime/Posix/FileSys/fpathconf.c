#include "platform.h"

C_Errno_t(C_Long_t) Posix_FileSys_fpathconf (C_Fd_t f, C_Int_t n) {
  return fpathconf (f, n);
}
