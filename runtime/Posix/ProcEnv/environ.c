#include "platform.h"

/* Manual initialization is a work-around for a Darwin linker issue. */
CstringArray Posix_ProcEnv_environ = 0;

