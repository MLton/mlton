#include "platform.h"

Int Posix_FileSys_fchown (Fd f, Uid u, Gid g) {
	return fchown (f, u, g);
}
