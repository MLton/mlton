#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Int Posix_ProcEnv_setenv (NullString s, NullString v) {
	return setenv ((char *)s, (char *)v, 1);
}
