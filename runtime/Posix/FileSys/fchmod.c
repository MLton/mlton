#include "platform.h"

Int Posix_FileSys_fchmod (Fd f, Mode m) {
	return fchmod (f, m);
}
