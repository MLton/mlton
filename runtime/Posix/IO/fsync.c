#include "platform.h"

Int Posix_IO_fsync (Fd f) {
	return fsync (f);
}
