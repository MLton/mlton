#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_IO_fcntl3 (Fd f, Int i, Int j) {
	return fcntl (f, i, j);
}
