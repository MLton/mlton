#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_pathconf(NullString p, Int n) {
	return pathconf((char *) p, n);
}
