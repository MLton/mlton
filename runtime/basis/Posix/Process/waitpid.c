#include "platform.h"

C_Errno_t(C_PId_t) Posix_Process_waitpid (C_PId_t p, Ref(C_Int_t) s, C_Int_t i) {
  return waitpid (p, (int*)s, i);
}
