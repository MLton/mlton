#include <sys/stat.h>
#include <sys/types.h>
#include "mlton-posix.h"

Int Posix_FileSys_fchmod(Fd f, Mode m) {
	return fchmod(f, m);
}
