#include "platform.h"

Bool Posix_ProcEnv_isatty (C_Fd_t f) {
  return 0 != isatty (f);
}
