#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Fd Posix_IO_dup2 (Fd f1, Fd f2) {
	return dup2 (f1, f2);
}
