#include <errno.h>
#include "mlton-posix.h"

void Posix_Error_clearErrno() {
	errno = 0;
}
