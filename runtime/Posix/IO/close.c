#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_IO_close (Fd f) {
	return close (f);
}
