#include <unistd.h>
#include "mlton-posix.h"

Int Posix_Process_pause() {
	return pause();
}
