#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Cstring Posix_ProcEnv_ttyname (Fd f) {
	return (Cstring)(ttyname (f));
}
