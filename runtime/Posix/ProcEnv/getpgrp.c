#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Pid Posix_ProcEnv_getpgrp () {
	return getpgrp ();
}
