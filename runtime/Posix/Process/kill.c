#include <signal.h>
#include "mlton-posix.h"

Int Posix_Process_kill(Pid p, Signal s) {
	return kill(p, s);
}
