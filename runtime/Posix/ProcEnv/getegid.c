#include <sys/types.h>
#include <unistd.h>
#include "mlton-posix.h"

Gid Posix_ProcEnv_getegid() {
	return getegid();
}
