#include <unistd.h>
#include "mlton-posix.h"

Ssize Posix_IO_read(Fd fd, Pointer b, Int i, Size s) {
	return (Ssize)(read(fd, (void *) ((char *) b + i), s));
}
