#include <unistd.h>
#include "mlton-posix.h"

Int Posix_ProcEnv_setuid(Uid u) {
	return setuid(u);
}
