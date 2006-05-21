#include "platform.h"

Bool_t Posix_ProcEnv_isatty (C_Fd_t f) {
  return isatty (f);
}
