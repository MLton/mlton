#include <unistd.h>
#include "mlton-posix.h"

Cstring Posix_ProcEnv_getlogin() {
	return (Cstring)(getlogin());
}
