#include <errno.h>
#include "mlton-posix.h"

int Posix_Error_getErrno() {
	return errno;
}
