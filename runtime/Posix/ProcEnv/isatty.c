#include <unistd.h>
#include "mlton-posix.h"

Bool Posix_ProcEnv_isatty(Fd f) {
	return isatty(f);
}
