#include "platform.h"

C_Int_t Posix_Process_ifSignaled (C_Status_t s) {
  return WIFSIGNALED (s);
}
