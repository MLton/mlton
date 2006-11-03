#include "platform.h"

C_Int_t Posix_Process_ifStopped (C_Status_t s) {
  return WIFSTOPPED (s);
}
