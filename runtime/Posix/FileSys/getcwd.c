#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Cstring Posix_FileSys_getcwd (Pointer buf, Size n) {
	return (Cstring)(getcwd (buf, n));
}
