#include "platform.h"

Int Posix_FileSys_ftruncate (Fd f, Position n) {
	return ftruncate (f, n);
}
