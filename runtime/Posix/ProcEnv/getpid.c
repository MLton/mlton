#include <unistd.h>
#include "mlton-posix.h"

Pid Posix_ProcEnv_getpid() {
	return getpid();
}
