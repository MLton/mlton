#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

void Posix_Process_exit (Int i) {
	exit (i);
}
