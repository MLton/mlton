#include <unistd.h>
#include "mlton-posix.h"

Int Posix_IO_pipe (Pointer fds) {
	return pipe ((int *) fds);
}
