#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Bool Posix_Process_ifExited (Status s) {
	int i;

	i = s;
	return WIFEXITED (i);
}
