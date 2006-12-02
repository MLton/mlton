#include "platform.h"

C_Int_t Posix_ProcEnv_isatty (C_Fd_t f) {
  return isatty (f);
}
