#include <unistd.h>
#include "mlton-posix.h"

Word Posix_FileSys_unlink(NullString p) {
	return unlink((char *) p);
}
