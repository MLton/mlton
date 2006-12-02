#include "platform.h"

/* Manual initialization is a work-around for a Darwin linker issue. */
C_Int_t CommandLine_argc = 0;
C_StringArray_t CommandLine_argv = (C_StringArray_t)NULL;
C_String_t CommandLine_commandName = (C_String_t)NULL;
