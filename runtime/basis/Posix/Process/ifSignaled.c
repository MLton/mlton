#include "platform.h"

Bool Posix_Process_ifSignaled (C_Status_t s) {
  int i;
  
  i = s;
  return 0 != WIFSIGNALED (i);
}
