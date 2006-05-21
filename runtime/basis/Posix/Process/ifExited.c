#include "platform.h"

Bool Posix_Process_ifExited (C_Status_t s) {
  int i;
  
  i = s;
  return WIFEXITED (i);
}
