#include <sys/stat.h>
#include <unistd.h>
#include "mlton-posix.h"

Bool Posix_FileSys_ST_isDir(Word w) {
	return S_ISDIR(w);
}
