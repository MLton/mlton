#include "platform.h"

enum {
	DEBUG_WRITE = FALSE,
};

Ssize Posix_IO_write (Fd fd, Pointer b, Int i, Size s) {
	Ssize res;
	
	res = (Ssize)(write (fd, (void *) ((char *) b + i), s));
	if (DEBUG_WRITE)
		fprintf (stderr, "%d = Posix_IO_write (%d, 0x%08x, %d, %d)\n",
				(int)res, (int)fd, (uint)b, (int)i, (int)s);
	return res;
}
