#include <unistd.h>
#include "mlton-posix.h"

Int Posix_ProcEnv_setgid(Gid g) {
	return setgid(g);
}
