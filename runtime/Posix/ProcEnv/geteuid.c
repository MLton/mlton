#include <sys/types.h>
#include <unistd.h>
#include "mlton-posix.h"

Uid Posix_ProcEnv_geteuid() {
	return geteuid();
}
