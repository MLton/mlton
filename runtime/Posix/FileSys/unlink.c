#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Word Posix_FileSys_unlink (NullString p) {
	return unlink ((char *) p);
}
