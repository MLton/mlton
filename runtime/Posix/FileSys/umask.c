#include <sys/types.h>
#include <sys/stat.h>
#include "mlton-posix.h"

Word Posix_FileSys_umask(Word w) {
	return umask(w);
}
