#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Pid Posix_ProcEnv_getpgrp () {
	return getpgrp ();
}
