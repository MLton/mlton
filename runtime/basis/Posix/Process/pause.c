#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_pause (void) {
  return pause ();
}
