#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Word Posix_FileSys_umask (Word w) {
	return umask (w);
}
