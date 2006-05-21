#include "platform.h"

C_UInt_t Posix_Process_alarm (C_UInt_t i) {
  return alarm (i);
}
