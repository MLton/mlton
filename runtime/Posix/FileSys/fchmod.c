#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_FileSys_fchmod (Fd f, Mode m) {
	return fchmod (f, m);
}
