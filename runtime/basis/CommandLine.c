#include "platform.h"

/* Manual initialization is a work-around for a Darwin linker issue. */
Int CommandLine_argc = 0;
CstringArray CommandLine_argv = 0;
Cstring CommandLine_commandName = 0;
