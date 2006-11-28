#include "platform.h"

C_Int_t Posix_Process_exitStatus (C_Status_t s) {
  int i;

  i = s;
  return WEXITSTATUS (i);
}
