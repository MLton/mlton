#include <unistd.h>
#include "mlton-posix.h"

Int Posix_Process_alarm(Int i) {
	return alarm(i);
}
