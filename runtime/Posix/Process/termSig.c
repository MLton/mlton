#include <wait.h>
#include "mlton-posix.h"

Signal Posix_Process_termSig(Status s) {
	return WTERMSIG(s);
}
