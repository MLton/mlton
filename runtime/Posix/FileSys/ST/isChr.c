#include <sys/stat.h>
#include <unistd.h>
#include "mlton-posix.h"

Bool Posix_FileSys_ST_isChr(Word w) {
	return S_ISCHR(w);
}
