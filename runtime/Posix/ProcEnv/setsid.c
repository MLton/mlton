#include <unistd.h>
#include "mlton-posix.h"

Pid Posix_ProcEnv_setsid() {
	return setsid();
}
