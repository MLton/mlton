#include <fcntl.h>
#include "mlton-posix.h"

Int Posix_IO_fcntl2 (Fd f, Int i) {
	return fcntl (f, i);
}
