#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Position Posix_IO_lseek (Fd f, Position i, Int j) {
	return lseek (f, i, j);
}
