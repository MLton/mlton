#include "platform.h"

Fd Posix_IO_dup2 (Fd f1, Fd f2) {
	return dup2 (f1, f2);
}
