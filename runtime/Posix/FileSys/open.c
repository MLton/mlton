#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "mlton-posix.h"

Int Posix_FileSys_open(NullString p, Word w, Mode m) {
	return open((char *) p, w, m);
}
