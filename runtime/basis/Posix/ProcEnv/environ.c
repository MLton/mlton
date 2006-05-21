#include "platform.h"

/* Manual initialization is a work-around for a Darwin linker issue. */
C_StringArray_t Posix_ProcEnv_environ = (C_StringArray_t)NULL;
