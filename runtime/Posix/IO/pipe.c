#include "platform.h"

C_Errno_t(C_Int_t) Posix_IO_pipe (Vector(C_Fd_t) fds) {
  return pipe ((int *) fds);
}
