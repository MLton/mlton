#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_rmdir(NullString p) {
	return rmdir((char *) p);
}
