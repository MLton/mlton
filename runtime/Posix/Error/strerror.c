#include <string.h>
#include "mlton-posix.h"

Cstring Posix_Error_strerror(Syserror n) {
	return (Cstring)(strerror(n));
}
