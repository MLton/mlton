#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Signal Posix_Process_termSig (Status s) {
	int i;

	i = s;
	return WTERMSIG (i);
}
