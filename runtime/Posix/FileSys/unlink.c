#include "platform.h"

Word Posix_FileSys_unlink (NullString p) {
	return unlink ((char *) p);
}
