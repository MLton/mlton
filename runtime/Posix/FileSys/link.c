#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Int Posix_FileSys_link (NullString p1, NullString p2) {
	return link ((char *) p1, (char *) p2);
}
