#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_ftruncate (Fd f, Position n) {
	return ftruncate (f, n);
}
