#include "platform.h"

Bool Posix_ProcEnv_isatty (Fd f) {
	return isatty (f);
}
