#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Int Posix_FileSys_mkdir (NullString p, Word w) {
	return mkdir2 ((char *) p, w);
}
