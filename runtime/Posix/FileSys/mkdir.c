#include <sys/stat.h>
#include "mlton-posix.h"

Int Posix_FileSys_mkdir(NullString p, Word w) {
	return mkdir((char *) p, w);
}
