#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Bool Posix_Process_ifStopped (Status s) {
	int i;

	i = s;
	return WIFSTOPPED (i);
}
