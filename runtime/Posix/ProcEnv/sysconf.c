#include <unistd.h>
#include "mlton-posix.h"

Int Posix_ProcEnv_sysconf(Int i) {
	return sysconf(i);
}
