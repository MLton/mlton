#include "platform.h"

C_Errno_t(C_Int_t) Posix_ProcEnv_getgroups (C_Int_t i, Array(C_GId_t) a) {
  return getgroups (i, (gid_t*)a);
}
