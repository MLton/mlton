#include "platform.h"

Pid Posix_ProcEnv_getpgrp () {
	return getpgrp ();
}
