#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Fd Posix_IO_dup (Fd f) {
	return dup( f);
}
