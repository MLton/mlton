#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Bool Posix_Process_ifSignaled (Status s) {
	int i;

	i = s;
	return WIFSIGNALED (i);
}
