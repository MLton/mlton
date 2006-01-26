#include "platform.h"

C_Errno_t(C_Long_t) Posix_ProcEnv_sysconf (C_Int_t i) {
  return sysconf (i);
}
