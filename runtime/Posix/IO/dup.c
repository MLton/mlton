#include "platform.h"

Fd Posix_IO_dup (Fd f) {
	return dup( f);
}
