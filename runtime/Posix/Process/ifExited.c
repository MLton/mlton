#include "wait.h"
#include "mlton-posix.h"

Bool Posix_Process_ifExited(Status s) {
	return WIFEXITED(s);
}
