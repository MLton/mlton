#include <unistd.h>
#include "mlton-posix.h"

Int Posix_Process_sleep (Int i) {
	return sleep (i);
}
