#include <sys/types.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_IO_lseek(Fd f, Int i, Int j) {
	return lseek(f, i, j);
}
