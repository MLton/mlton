#include "platform.h"

Int Posix_FileSys_chown (NullString p, Uid u, Gid g) {
	return chown ((char *) p, u, g);
}
