#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_IO_fcntl2 (Fd f, Int i) {
	return fcntl (f, i);
}
