#include <wait.h>
#include "mlton-posix.h"

Bool Posix_Process_ifStopped(Status s) {
	return WIFSTOPPED(s);
}
