#include "platform.h"

Word Posix_FileSys_umask (Word w) {
	return umask (w);
}
