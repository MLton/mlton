#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_FileSys_pathconf (NullString p, Int n) {
	return pathconf ((char *)p, n);
}
