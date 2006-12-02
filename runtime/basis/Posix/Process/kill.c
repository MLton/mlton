#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_kill (C_PId_t p, C_Signal_t s) {
  return kill (p, s);
}
