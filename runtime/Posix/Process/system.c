#include "platform.h"

C_Errno_t(C_Status_t) Posix_Process_system (NullString8_t cmd) {
  return system ((const char*) cmd);
}
