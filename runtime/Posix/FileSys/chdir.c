#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_chdir(Cpointer p) {
	return chdir((char *) p);
}
