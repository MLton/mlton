#include "platform.h"

Int Posix_FileSys_mkfifo (NullString p, Word w) {
	return mkfifo ((char *) p, w);
}
