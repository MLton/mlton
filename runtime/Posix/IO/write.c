#include <unistd.h>
#include "mlton-posix.h"

Ssize Posix_IO_write(Fd fd, Pointer b, Int i, Size s) {

	return (Ssize)(write(fd, (void *) ((char *) b + i), s));
}
