#include "platform.h"

C_Mode_t Posix_FileSys_umask (C_Mode_t m) {
  return umask (m);
}
