#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Fd Posix_IO_dup (Fd f) {
	return dup( f);
}
