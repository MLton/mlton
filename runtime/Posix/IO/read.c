#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Ssize Posix_IO_read (Fd fd, Pointer b, Int i, Size s) {
	return (Ssize)(read (fd, (void *) ((char *) b + i), s));
}
