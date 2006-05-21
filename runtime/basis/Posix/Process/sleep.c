#include "platform.h"

C_UInt_t Posix_Process_sleep (C_UInt_t i) {
  return sleep (i);
}
