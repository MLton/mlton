#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_FileSys_rmdir (NullString p) {
	return rmdir ((char *) p);
}
