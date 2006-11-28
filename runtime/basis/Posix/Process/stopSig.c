#include "platform.h"

C_Signal_t Posix_Process_stopSig (C_Status_t s) {
  int i;

  i = s;
  return WSTOPSIG (i);
}
