#include <unistd.h>
#include "mlton-posix.h"

Int Posix_IO_fsync(Fd f) {
	return fsync(f);
}
