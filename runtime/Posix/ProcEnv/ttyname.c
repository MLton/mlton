#include <unistd.h>
#include "mlton-posix.h"

Cstring Posix_ProcEnv_ttyname(Fd f) {
	return (Cstring)ttyname(f);
}
