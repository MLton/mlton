#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_ftruncate(Fd f, Int n) {
	return ftruncate(f, n);
}
