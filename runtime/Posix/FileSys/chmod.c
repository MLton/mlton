#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Int Posix_FileSys_chmod (NullString p, Mode m) {
	return chmod ((char *) p, m);
}
