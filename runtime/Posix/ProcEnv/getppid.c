#include <unistd.h>
#include "mlton-posix.h"

Pid Posix_ProcEnv_getppid() {
	return getppid();
}
