#include "platform.h"

Int Posix_FileSys_mkdir (NullString p, Word w) {
	return mkdir2 ((char *) p, w);
}
