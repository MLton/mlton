#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Signal Posix_Process_stopSig (Status s) {
	int i;

	i = s;
	return WSTOPSIG (i);
}
