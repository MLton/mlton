#include <unistd.h>
#include <sys/types.h>
#include "mlton-posix.h"

Gid Posix_ProcEnv_getgid() {
	return getgid();
}
