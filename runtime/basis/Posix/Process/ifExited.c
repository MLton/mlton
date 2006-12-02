#include "platform.h"

C_Int_t Posix_Process_ifExited (C_Status_t s) {
  return WIFEXITED (s);
}
