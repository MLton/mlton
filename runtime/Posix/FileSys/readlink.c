#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_readlink(NullString p, Pointer b, Int n) {
	return readlink((char *) p, b, n);
}
