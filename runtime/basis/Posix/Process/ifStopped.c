#include "platform.h"

Bool Posix_Process_ifStopped (C_Status_t s) {
  int i;
  
  i = s;
  return 0 != WIFSTOPPED (i);
}
