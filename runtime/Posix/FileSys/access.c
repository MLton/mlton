#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_access(NullString f, Word w) {
	return access((char *) f, w);
}
