#include <unistd.h>
#include "mlton-posix.h"

Int Posix_IO_close (Fd f) {
	return close (f);
}
