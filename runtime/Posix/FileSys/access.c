#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_FileSys_access (NullString f, Word w) {
	return access ((char *) f, w);
}
