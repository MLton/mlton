#include <unistd.h>
#include "mlton-posix.h"

Int Posix_ProcEnv_setpgid(Pid p, Gid g) {
	return setpgid(p, g);
}
