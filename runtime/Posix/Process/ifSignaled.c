#include <wait.h>
#include "mlton-posix.h"

Bool Posix_Process_ifSignaled(Status s) {
	return WIFSIGNALED(s);
}
