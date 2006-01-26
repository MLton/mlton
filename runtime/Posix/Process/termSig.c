#include "platform.h"

C_Signal_t Posix_Process_termSig (C_Status_t s) {
  int i;
  
  i = s;
  return WTERMSIG (i);
}
