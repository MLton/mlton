#include <sys/wait.h>
#include "mlton-posix.h"

Pid Posix_Process_waitpid(Pid p, Pointer s, Int i) {
	return waitpid(p, (int*)s, i);
}
