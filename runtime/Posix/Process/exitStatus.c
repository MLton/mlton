#include <sys/wait.h>
#include "mlton-posix.h"

Int Posix_Process_exitStatus(Status s) {
	return WEXITSTATUS(s);
}
