#include <unistd.h>
#include <sys/types.h>
#include "mlton-posix.h"

Uid Posix_ProcEnv_getuid() {
	return getuid();
}
