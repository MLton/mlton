#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Cstring Posix_ProcEnv_getlogin () {
	return (Cstring)(getlogin ());
}
