#include <sys/stat.h>
#include <sys/types.h>
#include "mlton-posix.h"

Int Posix_FileSys_chmod(NullString p, Mode m) {
	return chmod((char *) p, m);
}
