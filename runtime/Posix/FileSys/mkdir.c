#include "platform.h"

Int Posix_FileSys_mkdir (NullString p, Word w) {
	return mkdir((char *) p, w);
}
