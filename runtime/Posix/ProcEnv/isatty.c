#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Bool Posix_ProcEnv_isatty (Fd f) {
	return isatty (f);
}
