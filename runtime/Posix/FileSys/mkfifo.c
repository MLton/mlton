#include <sys/stat.h>
#include "mlton-posix.h"

Int Posix_FileSys_mkfifo(NullString p, Word w) {
	return mkfifo((char *) p, w);
}
