#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_link(NullString p1, NullString p2) {
	return link((char *) p1, (char *) p2);
}
