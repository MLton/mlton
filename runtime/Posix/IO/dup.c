#include <unistd.h>
#include "mlton-posix.h"

Fd Posix_IO_dup (Fd f) {
	return dup( f);
}
