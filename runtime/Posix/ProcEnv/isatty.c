#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Bool Posix_ProcEnv_isatty (Fd f) {
	return isatty (f);
}
