#include "platform.h"

Int Posix_IO_fcntl2 (Fd f, Int i) {
	return fcntl (f, i);
}
