#include <unistd.h>
#include "mlton-posix.h"

Pid Posix_Process_fork() {
	return fork();
}
