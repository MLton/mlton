#include <sys/types.h>
#include <unistd.h>
#include "mlton-posix.h"

Position Posix_IO_lseek (Fd f, Position i, Int j) {
	return lseek (f, i, j);
}
