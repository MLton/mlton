#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

/* Manual initialization is a work-around for a Darwin linker issue. */
CstringArray Posix_ProcEnv_environ = 0;

